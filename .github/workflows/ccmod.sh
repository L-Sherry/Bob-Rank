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

# zip everything, hopefully the zip is reproducible
name="${1##*/}"
version="${2##*/}"
output="$name-$version.ccmod"
rm -f "$output"

printf 'Creating ccmod for %s version %s (%s)\n' "$name" "$version" "$output"

zip "$output" --must-match --no-wild -X -@ << EOF
$filelist
EOF

# tell github the name of the zip
printf '::set-output name=zip_name::%s\n' "$output"
