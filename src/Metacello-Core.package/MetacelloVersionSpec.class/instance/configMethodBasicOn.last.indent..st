printing
configMethodBasicOn: aStream last: last indent: indent
    | values lastIndex lastBlock |
    last
        ifTrue: [ 
            "need to calculate last statement with a value"
            values := {(self getBlessing).
            (self getDescription).
            (self getPreLoadDoIt).
            (self getPostLoadDoIt).
            (self getAuthor).
            (self getTimestamp)}.
            1 to: values size do: [ :index | 
                (values at: index) ~~ nil
                    ifTrue: [ lastIndex := index ] ].
            lastBlock := [ :arg | arg = lastIndex ] ]
        ifFalse: [ lastBlock := [ :arg | false ] ].
    self
        configMethodValueOn: aStream
        for: self getBlessing
        selector: 'blessing:'
        last: (lastBlock value: 1)
        indent: indent.
    self
        configMethodValueOn: aStream
        for: self getDescription
        selector: 'description:'
        last: (lastBlock value: 2)
        indent: indent.
    self
        configMethodValueOn: aStream
        for: self getPreLoadDoIt
        selector: 'preLoadDoIt:'
        last: (lastBlock value: 3)
        indent: indent.
    self
        configMethodValueOn: aStream
        for: self getPostLoadDoIt
        selector: 'postLoadDoIt:'
        last: (lastBlock value: 4)
        indent: indent.
    self
        configMethodValueOn: aStream
        for: self getAuthor
        selector: 'author:'
        last: (lastBlock value: 5)
        indent: indent.
    self
        configMethodValueOn: aStream
        for: self getTimestamp
        selector: 'timestamp:'
        last: (lastBlock value: 6)
        indent: indent