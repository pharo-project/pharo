github support
extractRepositoryFrom: zipFile to: directory
    "unzip <zipFile> into <directory>"
    ZipArchive new
        readFrom: zipFile;
        extractAllTo: (FileDirectory on: directory) informing: nil overwrite: true.