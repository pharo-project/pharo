private
basicLoad
    errorDefinitions := OrderedCollection new.
    SourceFiles deferFlushDuring: [
        self handleLoadErrorsDuring: [self basicLoadDefinitions]]