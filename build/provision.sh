#!/usr/bin/env bash

# get resources:

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)"
pushd "$DIR">/dev/null

# files
files=("http://nand2tetris.org/tutorials/PDF/Hardware%20Simulator%20Tutorial.pdf"
       "http://www.nand2tetris.org/chapters/chapter%2001.pdf"
       "http://www.nand2tetris.org/chapters/appendix%20A.pdf"
       "http://www.nand2tetris.org/software/HDL%20Survival%20Guide.html")

# repos
declare -A repos=()

# get software suite
nand_bundle="http://nand2tetris.org/software/nand2tetris.zip"
get_nand_bundle () {
    if [ ! -d "nand2tetris" ]; then
        wget $nand_bundle
        7za x -tzip "nand2tetris.zip"
        rm nand2tetris.zip
    fi
}

# get files
get_resource_files () {
    for f in ${files[@]}; do
        if [[ ! -f $(basename $f) ]]; then
            wget $f
        fi
    done
}

# get / update resource repos
get_resource_repos () {
    for repo in "${!repos[@]}"; do
        if [[ ! -d "${repos["$repo"]}" ]]; then
            git clone --depth 1 "$repo" "${repos["$repo"]}"
        else
            pushd "${repos["$repo"]}">/dev/null
            git pull --depth 1
            popd>/dev/null
        fi
    done
}

# ------------------------------------------------------------
# get stuff

get_nand_bundle
# get_resource_files
get_resource_repos

popd>/dev/null

# Local Variables:
# sh-shell: bash
# End:
