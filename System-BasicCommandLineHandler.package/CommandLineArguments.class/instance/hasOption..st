testing
hasOption: aString
	| option |
	option := (aString size = 1
					ifTrue: [ '-']
					ifFalse: [ '--']), aString.
	(self arguments includes: option) ifTrue: [ ^ true ].
	option := option,'='.
	^ self arguments anySatisfy: [ :arg| arg beginsWith: option ].