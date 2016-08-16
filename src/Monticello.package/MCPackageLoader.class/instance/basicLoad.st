private
basicLoad
    errorDefinitions := OrderedCollection new.
    SourceFiles deferFlushDuring: [self basicLoadDefinitions].