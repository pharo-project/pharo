handlers
handleLookupProjectSpec: exception
    ^ exception
        resume:
            ((self lookupProjectSpecFor: exception projectSpec) ifNil: [ ^ exception resume: exception projectSpec ])