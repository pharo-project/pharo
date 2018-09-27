I represent the version of the pharo image to build.
I contain

 - major version (mandatory)
 - minor version (optional, 0 if absent)
 - patch version (optional, 0 if absent)
 - suffix  (optional, nil if absent)
 - commit hash (optional, 'UNKNOWN_COMMIT' if absent)
 - build number (optional, 'UNKNOWN_BUILD' if absent)

I am used during the bootstrap process to parse this information and include it in the generated binary image.