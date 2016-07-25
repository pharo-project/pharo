method accessing
existingMethodsThatReferToClassVariable: aString
	| binding existingMethods |
	binding := (self bindingOf: aString)
		ifNil: [ ^ #() ].
	existingMethods := self realClass
		whichSelectorsReferTo: binding.
	(newMethods isNil and: [ removedMethods isNil ])
		ifTrue: [ ^ existingMethods ].
	^ existingMethods
		reject: [ :each | (self hasRemoved: each) or: [ self newMethods includesKey: each ] ]