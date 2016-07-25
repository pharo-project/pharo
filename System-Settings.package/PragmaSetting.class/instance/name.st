accessing
name
	^ name ifNil: [super name] ifNotNil: [name value]