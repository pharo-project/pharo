method accessing
whichSelectorsReferToInstanceVariable: aString 
	| selectors |
	selectors := Set new.
	newMethods isNil ifFalse: [
		newMethods do: [ :each | 
			(each refersToVariable: aString)
				ifTrue: [ selectors add: each selector ] ] ].
	self isDefined 
		ifTrue: [ selectors addAll: (self existingMethodsThatReferToInstanceVariable: aString) ].
	^ selectors