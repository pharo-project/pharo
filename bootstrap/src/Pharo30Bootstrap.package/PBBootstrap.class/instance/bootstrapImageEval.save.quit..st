image-management
bootstrapImageEval: aString save: save quit: quit
	
	| arguments |
	arguments := { 'eval' . aString } asOrderedCollection.
	save ifTrue: [ arguments addLast: '--save' ].
	quit ifTrue: [ arguments addLast: '--quit' ].
	^ self bootstrapImageDo: arguments asArray