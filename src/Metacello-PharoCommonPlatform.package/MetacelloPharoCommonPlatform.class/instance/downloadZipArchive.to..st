github support
downloadZipArchive: url to: outputFileName
  "download zip archive from <url> into <outputFileName>"

    | archive zipfile |
    self downloadBasicFile: url to: outputFileName username: nil pass: nil.
    archive := ZipArchive new.
    zipfile := self fileHandleOn: outputFileName.
    zipfile containingDirectory readOnlyFileNamed: zipfile localName do: [ :fileStream | archive readFrom: fileStream ].
    ^ archive
