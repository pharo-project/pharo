instance creation
current
    ^ Current ifNil: [ Current := self new ]