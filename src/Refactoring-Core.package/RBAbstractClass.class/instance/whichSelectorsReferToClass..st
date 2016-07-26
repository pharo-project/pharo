method accessing
whichSelectorsReferToClass: aRBClass 
	| selectors |
	selectors := Set new.
	newMethods isNil ifFalse: [
		newMethods do: [ :each | 
			(each refersToClassNamed: aRBClass name) 
				ifTrue: [ selectors add: each selector ] ] ].
	(self isDefined and: [ aRBClass isDefined ]) ifTrue: [
		selectors addAll: (self existingMethodsThatReferTo: aRBClass classBinding).
		selectors addAll: (self existingMethodsThatReferTo: aRBClass name) ].
	^ selectors