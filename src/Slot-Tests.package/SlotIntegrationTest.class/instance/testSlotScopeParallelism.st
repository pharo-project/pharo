tests
testSlotScopeParallelism
	"Proposed by Camille Teruel"

	| classesAndMetaclasses candidates classWithWrongSlotScope |
	classesAndMetaclasses := Smalltalk allClasses flatCollect: [ :each | { each. each class } ].

	candidates := classesAndMetaclasses reject: [ :each | 
		each superclass isNil or: [ 
			each classLayout slotScope isKindOf: LayoutEmptyScope ] ].

	classWithWrongSlotScope := candidates reject: [ :each | 
		each superclass classLayout slotScope == each classLayout slotScope parentScope ].

	self assert: classWithWrongSlotScope asArray equals: #()