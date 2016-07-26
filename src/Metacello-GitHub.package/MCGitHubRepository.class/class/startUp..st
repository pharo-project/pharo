system startup
startUp: isImageStarting
    "Flush the GitHub download cache"

    isImageStarting
        ifTrue: [ self flushDownloadCache ]