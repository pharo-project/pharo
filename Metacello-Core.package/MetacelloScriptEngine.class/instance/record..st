actions api
record: required
    self
        fetchRecord: [ :version | 
            required isEmpty
                ifTrue: [ version record ]
                ifFalse: [ version record: required ] ]
        required: required