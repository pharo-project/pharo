method accessing
existingMethodsThatReferToInstanceVariable: aString
	| existingMethods |
	existingMethods := self realClass 
		whichSelectorsAccess: aString.
	(newMethods isNil and: [ removedMethods isNil ])
		ifTrue: [ ^ existingMethods ].
	^ existingMethods 
		reject: [ :each | (self hasRemoved: each) or: [ self newMethods includesKey: each ] ]