image-management
bootstrapImageEvalFile: aString save: save quit: quit
	
	| arguments |
	arguments := { 'st' . aString } asOrderedCollection.
	save ifTrue: [ arguments addLast: '--save' ].
	quit ifTrue: [ arguments addLast: '--quit' ].
	^ self bootstrapImageDo: arguments asArray