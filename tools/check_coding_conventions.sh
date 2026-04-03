#!/bin/bash

#set -x
set -e
set -o pipefail #abort if left command on a pipe fails

#This script checks:
# - the presence of IMPLICIT NONE
# - that all dummy arguments are declared using the INTENT attribute
# - that there is no (:) in subroutine call
# - the absence of unused local variables
# - that mode_ice4_pack and mode_ice4_stepping have the same interface

PHYEXTOOLSDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

SOURCEDIR=$PHYEXTOOLSDIR/../src/common

################################
#### COMMAND LINE ARGUMENTS ####
################################

verbose=0
function usage {
  echo "Usage: $0 [-h] [-v]"
  echo "-v  print non conforming messages"
}
while [ -n "$1" ]; do
  case "$1" in
    '-h') usage; exit;;
    '-v') verbose=1;;
    '--source') SOURCEDIR=$2; shift;;
    *) echo "Too many arguments"; exit 3;;
  esac
  shift
done

###############
#### SETUP ####
###############

JSONDIR=$(mktemp -d)
trap "\rm -rf $JSONDIR" EXIT
set +e
which pyfortool_parallel > /dev/null
ret=$?
set -e
if [ $ret != 0 ]; then
  echo "pyfortool_parallel not found!"
  exit 1
fi

################
#### CHECKS ####
################

check=true
result_all=""

# Check interfaces of mode_ice4_pack and mode_ice4_stepping
result=$(python3 -c "
import pyfortool
def args(filename):
  vars = [v for v in pyfortool.PYFT(filename).varList if v['arg']]
  vars = sorted(vars, key=lambda v: v['argorder'])
  return [v['n'] for v in vars]
dummy_pack = args('$SOURCEDIR/micro/mode_ice4_pack.F90')
dummy_pack = [v for v in dummy_pack if v not in ('D', 'KSIZE')]
dummy_step = args('$SOURCEDIR/micro/mode_ice4_stepping.F90')
dummy_step = [v for v in dummy_step if v not in ('KMICRO', )]
if dummy_pack != dummy_step:
  print('mode_ice4_pack and mode_ice4_stepping have different interfaces!')
")
[ "$result" != "" ] && check=false
result_all="${result_all}\n${result}"

# Other tests
result=$(pyfortool_parallel --tree $SOURCEDIR --descTree $JSONDIR/tree.json \
                            --wrapH --enableCache \
                            --checkIMPLICIT Warn --checkINTENT Warn \
                            --checkPHYEXUnusedLocalVar Warn \
                            --checkEmptyParensInCall Warn 2>&1)
[ "$result" != "" ] && check=false
result_all="${result_all}\n${result}"

# Exit status
if [ $check == false ]; then
  [ $verbose -eq 1 ] && echo -e "${result_all}"
  exit 2
fi
