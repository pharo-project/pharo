testing
isSeparator
    | in |
    ^ (in := self asInteger) == 32 or: [ in == 13 or: [ in == 9 or: [ in == 10 or: [ in == 12 ] ] ] ]