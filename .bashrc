# Needed for readable color scheme in Emacs/Vim. Make sure PuTTY settings
# already support 256 colors
export TERM=xterm-256color
export TERMCAP=

source ${HOME}/.bash_aliases

export WORKSPACE="/local/mnt/workspace"
export WORKSPACE2="/local/mnt2/workspace2"
export WORKSPACE3="/local/mnt3/workspace3"

# SSH / Git
#
# To start ssh-agent once and have the SSH_AUTH_SOCK variable persist across new
# terminal sessions, a common approach involves setting a fixed socket path and
# ensuring the agent starts only if it's not already running.
#
# It first defines a fixed path for the SSH_AUTH_SOCK. Then, it checks if an
# ssh-agent process is already running. If not, it removes any existing socket
# file at the defined path and starts ssh-agent with the -a option to specify
# the socket path. Finally, it exports the SSH_AUTH_SOCK variable to ensure it's
# available in the current and subsequent shell sessions.
SSH_AUTH_SOCK="/tmp/ssh-agent.sock"
if ! pgrep -x "ssh-agent" > /dev/null; then
  rm -f "$SSH_AUTH_SOCK"
  ssh-agent -a "$SSH_AUTH_SOCK" > /dev/null
fi
export SSH_AUTH_SOCK

# Python
export PYTHONSTARTUP=${HOME}/.pythonrc
export PYTHONUSERBASE=${WORKSPACE}/python/.local
export PIP_CACHE_DIR=${WORKSPACE}/pip_cache
export PATH=${PYTHONUSERBASE}/bin:${PATH}

# alias python="/pkg/qct/software/python/3.6.0/bin/python"
# alias pydoc="/pkg/qct/software/python/3.6.0/bin/pydoc3"

# Emacs
export EDITOR="emacs"
# export EMACS_VERSION=26.2
# export EMACS_PATH="/pkg/qct/software/emacs/${EMACS_VERSION}/bin"
export DOOMDIR=~/dotfiles/emacs/config/doom
# Root directory for local storage. Use this as a storage location for this system's installation of Doom Emacs.
export DOOMLOCALDIR=${WORKSPACE}/doom/local/

# PATH alterations
# export PATH=${PATH}:/prj/qct/asw/qctss/linux/bin
# export PATH=${PATH}:.
export PATH=~/bin:${PATH}
export PATH=~/.local/bin:${PATH}
export PATH=/pkg/qct/software/perforce/bin:${PATH}


# Perforce
export PATH=${PATH}:/prj/qct/asw/qctss/linux/bin
export P4PORT=QCTP401:1666
export P4USER=`whoami`
export P4CONFIG=.p4config # dot p4config
export P4EDITOR=vi
alias  vce='/prj/qct/asw/qctss/linux/bin/vce/vce.py'
source ${HOME}/p4funcs.bash

# export PATH=/usr/sbin:~/bin:/sbin:/usr/local/sbin:/usr/local/lib:/prj/qct/asw/qctss/linux/bin:$PATH

export GREPOPTIONS=" --color=always -n --exclude=tags"
export PERL5LIB="/local/mnt/workspace/Git-FastExport-0.07/blib/lib/"

# SSG Setup
export WORKSPACE="/local/mnt/workspace"
export WORKSPACE2="/local/mnt2/workspace2"
export WORKSPACE3="/local/mnt3/workspace3"
export SECTOOLS="/pkg/sectools/v2/latest/Linux/sectools"

# Rust things
# Don't clog up /usr2/<userid> and ~/.snapshot with cargo / rustup bins
export CARGO_HOME=${WORKSPACE}/cargo
export RUSTUP_HOME=${WORKSPACE}/rustup # Move from default ($HOME), since it fills up ~/.snapshot
export PATH=$PATH:$CARGO_HOME/bin
# `rustc --print=sysroot` tells you where the llvm bins can be found
export PATH=${PATH}:${RUSTUP_HOME}/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/bin

if [ -d "/local2/mnt/workspace/.ccache" ]; then
    #echo "ccache is present in this machine"
    export CCACHE_DIR=/local2/mnt/workspace/.ccache
    #echo $CCACHE_DIR
    export USE_CCACHE=1
    #echo $USE_CCACHE
fi

function makeccache
{
    T=$(gettop);
    if [ "$T" ]; then
        $(gettop)/prebuilts/misc/linux-x86/ccache/ccache -M 10G
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function wccache
{
    T=$(gettop);
    if [ "$T" ]; then
        watch -n1 -d $(gettop)/prebuilts/misc/linux-x86/ccache/ccache -s
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

#functions to switch directies quickly
function kernel
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/kernel
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function qseecom
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/kernel/drivers/misc/ >& /dev/null
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function mdss
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/kernel/drivers/video/msm/mdss/ >& /dev/null
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function wfd
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/kernel/drivers/media/platform/msm/wfd/ >& /dev/null
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function vidc
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/kernel/drivers/media/platform/msm/vidc/ >& /dev/null
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function out
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/out/target/product/$TARGET_PRODUCT/;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function ion
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/kernel/drivers/gpu/ion/ >& /dev/null || pushd $(gettop)/kernel/drivers/staging/android/ion/ >& /dev/null
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function iommu
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/kernel/drivers/iommu/ >& /dev/null
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function dts
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/kernel/arch/arm/boot/dts/qcom/ >& /dev/null
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function configs
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/kernel/arch/arm64/configs/ >& /dev/null
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function av
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/frameworks/av;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function nuplayer
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/frameworks/av/media/libmediaplayerservice;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function sf
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/frameworks/base/media/libstagefright >& /dev/null || pushd $(gettop)/frameworks/av/media/libstagefright;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function vdec
{
    mmvideo
    pushd vidc/vdec/src/ >& /dev/null
}

function mmvideo
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/hardware/qcom/media/mm-video-v4l2/ >& /dev/null || pushd $(gettop)/hardware/qcom/media/mm-video >& /dev/null;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function display
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/hardware/qcom/display/ >& /dev/null
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function mmparser
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/vendor/qcom/proprietary/mm-parser/ >& /dev/null;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function avenh
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/vendor/qcom/proprietary/avenhancements/ >& /dev/null;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function c2dcc
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/hardware/qcom/media/libc2dcolorconvert/ >& /dev/null;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function mmcc
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/vendor/qcom/proprietary/mm-color-convertor/libmmcolorconvertor >& /dev/null;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function cts
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/cts/tests/tests/media/src/android/media/cts >& /dev/null;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function base
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/frameworks/base/media/java/android/media >& /dev/null;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function gpupp
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/vendor/qcom/proprietary/graphics-tests/android/gpupostprocessing >& /dev/null || pushd $(gettop)/vendor/qcom/proprietary/gles/adreno200/tools/gpupostprocessing >& /dev/null;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function testapp
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/vendor/qcom/proprietary/mm-video-utils/vtest-omx/ >& /dev/null || pushd $(gettop)/vendor/qcom/proprietary/mm-video-utils/vtest/ >& /dev/null || pushd $(gettop)/vendor/qcom/proprietary/mm-video-noship/vidc/vtest-omx/ >& /dev/null;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function widevine
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/vendor/widevine/proprietary/ >& /dev/null;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function securemsm
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/vendor/qcom/proprietary/securemsm-noship >& /dev/null;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function exoplayer
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/external/exoplayer >& /dev/null;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function bb
{
    pushd /local/mnt/workspace/shalajj/Build_Bot/ >& /dev/null || pushd /local/mnt/workspace2/shalajj/Build_Bot/ >& /dev/null || pushd /local/mnt/workspace/BuildBot/ >& /dev/null || pushd /local2/mnt/workspace/BuildBot/ >& /dev/null || pushd /local/mnt2/workspace2/BuildBot/ >& /dev/null || pushd /local2/mnt/workspace/shalajj/Build_Bot/ >& /dev/null;
}

function master
{
    bb
    pushd master/basedir/ >& /dev/null
}

function slave
{
    bb
    pushd slave/basedir/ >& /dev/null
}

function bborig
{
    pushd /usr/local/lib/python2.7/dist-packages/buildbot/ >& /dev/null;
}

function virtio
{
    pushd /local/mnt/workspace/shalajj/virtio_data/ >& /dev/null;
}

function work1
{
    pushd /local/mnt/workspace/ >& /dev/null;
}

function work2
{
    pushd /local/mnt/workspace2/ >& /dev/null || pushd /local2/mnt/workspace/ >& /dev/null || pushd /local/mnt2/mnt/workspace/ >& /dev/null;
}

function work3
{
    pushd /local/mnt2/workspace2/ >& /dev/null;
}

function repoinit
{
    branch=$1
    tag=$2
    if [ "$tag" ]; then
        echo "Using Tag";
        repo init -u git://git.quicinc.com/platform/manifest.git -b refs/tags/$tag -m versioned.xml
    else
        echo "Using Branch";
        repo init -u git://git.quicinc.com/platform/manifest.git -b $branch
    fi
}

function grebase
{
    git stash;
    repo start temp .;
    git fetch;
    git rebase;
}

function setenv
{
    target=$1
    source build/envsetup.sh
    lunch $target-userdebug
}

# TZ build cmnds #
function celerity
{
    T=$(gettop);
    W=$WS;
    if [ "$T" ]; then
        pushd $(gettop)/apps/securemsm/trustzone/qsapps/celerity >& /dev/null || pushd $(gettop)/vendor/qcom/proprietary/securemsm-internal/scipp >& /dev/null
    elif [ "$W" ]; then
        pushd $W/security/securemsm-noship/scipp >& /dev/null;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function licmngr
{
    T=$(gettop);
    W=$WS;
    if [ "$T" ]; then
        pushd $(gettop)/apps/securemsm/trustzone/qsapps/license_manager >& /dev/null || pushd $(gettop)/vendor/qcom/proprietary/securemsm-noship/LicenseManager >& /dev/null
    elif [ "$W" ]; then
        pushd $W/security/securemsm-noship/LicenseManager >& /dev/null;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function aptcrypto
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/apps/securemsm/trustzone/qsapps/aptcryptotestapp/src >& /dev/null;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function sampleapp
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/core/securemsm/trustzone/qsapps/sampleapp/src >& /dev/null;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function teetest
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/core/securemsm/trustzone/qsapps/teetest/teetest/src >& /dev/null;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function tzbuild
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/build/ms >& /dev/null;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function tzcore
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/core >& /dev/null;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function tzapps
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/apps >& /dev/null;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function tzssg
{
    T=$(gettop);
    if [ "$T" ]; then
        pushd $(gettop)/ssg >& /dev/null;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}

function poky
{
    T=$WS;
    if [ "$T" ]; then
        pushd $T/poky/build >& /dev/null;
    else
        echo "Couldn't locate the top of the tree.  Try setting TOP.";
    fi
}


function tzsetupproj
{
    project=$1
    if [ -d "$project" ]; then
        cd $project
        if [ -d ".git/" ]; then
            echo "$project already initialized"
        else
            echo $project
            cp ~/.gitignore_"$project" .gitignore
            git init
            git add .gitignore
            git commit -m "Initial commit with .gitignore"
            git add .
            git commit -m "code commit"
        fi
        cd ..
    else
        echo "Skipped: $project/ does not exist"
    fi
}

function tzsetup
{
    pushd trustzone_images
    source $WORKSPACE/scripts/tzsetenv.sh
    # tzsetupproj apps
    tzsetupproj core
    tzsetupproj build
    tzsetupproj uclib
    tzsetupproj ssg
    popd
}
