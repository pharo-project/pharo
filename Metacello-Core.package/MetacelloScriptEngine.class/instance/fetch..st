actions api
fetch: required
    self
        fetchRecord: [ :version | 
            required isEmpty
                ifTrue: [ version fetch ]
                ifFalse: [ version fetch: required ] ]
        required: required