*GT-SpotterExtensions-Core-private
recentItems
	recent isEmptyOrNil ifTrue: [ ^ #() ].
	^ recent allButFirst reject: [ :each | 
		each isEmptyOrNil 
			or: [ each asString trimBoth isEmptyOrNil ] ]