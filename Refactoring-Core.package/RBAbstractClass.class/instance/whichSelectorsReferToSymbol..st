method accessing
whichSelectorsReferToSymbol: aSymbol 
	| selectors |
	selectors := Set new.
	newMethods isNil ifFalse: [
		newMethods do: [ :each | 
			(each refersToSymbol: aSymbol)
				ifTrue: [ selectors add: each selector ] ] ].
	self isDefined 
		ifTrue: [ selectors addAll: (self existingMethodsThatReferTo: aSymbol) ].
	^ selectors