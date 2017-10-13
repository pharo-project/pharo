api - context
selectedMessageName

	^message ifNil:[super selectedMessageName] ifNotNil: [ message ]