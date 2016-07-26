github/bitbucket support
downloadErrorFileNameFor: zipFileName
  | pid |
  pid := self processPID.
  ^ '/tmp/curl-' , pid , '-' , (zipFileName select: [ :c | c isAlphaNumeric ])
    , '.err'