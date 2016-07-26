system startup
startUp: resuming
    "Flush the GitHub download cache"

    resuming
        ifTrue: [ self flushDownloadCache ]