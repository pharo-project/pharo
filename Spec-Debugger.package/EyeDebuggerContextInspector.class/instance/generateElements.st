list
generateElements
	"generate the collection of elements to show in the inspector for the object inspected"

	| elements |
	elements := OrderedCollection new.
	self addSelf: elements.
	"only if object is not nil this elements are retrievable"
	self object
		ifNotNil: [ 
			self addStackTop: elements.
			self addAllInstVars: elements.
			self addSpecialFields: elements.
			self addInstancesVariable: elements.
			self addVariableFields: elements ].
	^ elements