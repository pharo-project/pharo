tests
testDetach

	| category1 category2 morph attachedCategories |
	category1 := KMCategory named: #TestBlah.
	category2 := KMCategory named: #TestAnother.
	morph := BorderedMorph new.
	KMRepository default addCategory: category1.
	KMRepository default addCategory: category2.
	morph kmDispatcher reset.
	morph attachKeymapCategory: #TestBlah.
	morph attachKeymapCategory: #TestAnother.
	morph kmDispatcher targets size = 2 ifFalse: [ self error: 'should have one category attached' ].

	morph detachKeymapCategory: #TestBlah.	
	attachedCategories := morph kmDispatcher targets collect: [ :e | e category name ].
	self assert: attachedCategories asArray equals: { #TestAnother }.

	self should: [ morph detachKeymapCategory: #NonExistent ] raise: Error.