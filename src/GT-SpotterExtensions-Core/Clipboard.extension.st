Extension { #name : #Clipboard }

{ #category : #'*GT-SpotterExtensions-Core-private' }
Clipboard >> recentItems [
	recent isEmptyOrNil ifTrue: [ ^ #() ].
	^ recent allButFirst reject: [ :each | 
		each isEmptyOrNil 
			or: [ each asString trimBoth isEmptyOrNil ] ]
]
