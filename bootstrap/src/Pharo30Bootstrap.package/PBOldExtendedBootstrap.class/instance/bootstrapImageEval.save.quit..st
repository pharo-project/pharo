image-management
bootstrapImageEval: aString save: save quit: quit
	
	| arguments |
	arguments := { 'eval' } asOrderedCollection.
	save ifTrue: [ arguments addLast: '--save' ].
	quit ifTrue: [ arguments addLast: '--quit' ].
	arguments add: aString.
	^ self bootstrapImageDo: arguments asArray