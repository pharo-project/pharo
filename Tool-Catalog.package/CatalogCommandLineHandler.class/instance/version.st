accessing
version
	^ version ifNil: [ 
		version := (self hasOption: 'version')
			ifTrue: [ self optionAt: 'version' ]
			ifFalse: [ #stable ] ]