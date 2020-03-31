#!/bin/sh
set -eu

# what files do we want
list_files () {
	git ls-tree --name-only -r HEAD | \
		grep -E '^[^.].*\.(js|json|ogg|opus|webm|png|jpe?g)$'
}
filelist="$(list_files)"

# change their date to latest commit's author date.
export TZ=UTC
author_date="@$(git log -n 1 --date=unix --format=%ad)"
xargs -d '\n' touch --date="$author_date" << EOF
$filelist
EOF

# zip everything, hopefully the zips are reproducible
name="${1##*/}"
version="${2##*/}"
outputccmod="$name-$version.ccmod"
outputdir="$name-$version"
outputzip="$name-$version.zip"
rm -f "$outputccmod" "$outputzip" "$outputdir"

printf 'Creating zip and ccmod for %s version %s\n' "$name" "$version"

# first, create the ccmod
zip "$outputccmod" --must-match --no-wild -X -@ << EOF
$filelist
EOF

# now create the zip, under a subdirectory.

ln -fs . "$outputdir"


prefixed_filelist="$(sed -re "s,^,$outputdir/," << EOF
$filelist
EOF
)"

zip "$outputzip" --must-match --no-wild -X -@ << EOF
$prefixed_filelist
EOF

rm -f "$outputdir"

# tell github the name of the zip/ccmod
printf '::set-output name=ccmod_name::%s\n' "$outputccmod"
printf '::set-output name=zip_name::%s\n' "$outputzip"
