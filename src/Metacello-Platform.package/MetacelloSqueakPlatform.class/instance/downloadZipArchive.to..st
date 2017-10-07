github support
downloadZipArchive: url to: outputFileName
  "download zip archive from <url> into <outputFileName>"

    | archive zipfile response |
    response := WebClient httpGet: url.
    archive := ZipArchive new.
    zipfile := FileDirectory on: (FileDirectory default fullPathFor: outputFileName).
    zipfile containingDirectory assureExistence.
    response
         streamTo: ((zipfile containingDirectory forceNewFileNamed: zipfile localName) binary; yourself)
         size: response contentLength progress: nil.
    zipfile containingDirectory
		readOnlyFileNamed: zipfile localName
		do: [:fileStream | archive readFrom: fileStream].
    ^ archive
