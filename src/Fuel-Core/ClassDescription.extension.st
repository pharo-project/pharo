Extension { #name : 'ClassDescription' }

{ #category : '*Fuel-Core' }
ClassDescription >> fuelRecomputeSlotScopeWithTraits [
	"We only know about trait variables once the trait composition
	has been fully materialized. Now we need to update the slot scope
	with the information from traits and recompute the class
	format since it may currently define too many fields.
	If that is indeed the case, then existing instances will have been
	allocated with more fields than necessary, however, their layout
	definition defines fewer slots, so all operations on instance
	variables are safe.
	New instances will be created with the correct amount of fields
	allocated.
	
	Note that the format computed here only differs from the initially
	set format if variables were removed from the class.
	
	See FLBehaviorCluster>>installLayout:withFormat:variableNames:into:
	and FLVariablesMapping>>materializeTraitVariablesWith:."

	| classSlots traitSlots newSlotScope |
	(self traitComposition slots allSatisfy: [ :slot |
		self hasSlotNamed: slot name ]) ifTrue: [ ^ self ].

	classSlots := self classLayout slots.
	traitSlots := self traitComposition slots select: [ :traitSlot |
		classSlots noneSatisfy: [ :slot | slot name = traitSlot name ] ].

	newSlotScope := self classLayout slotScope parentScope extend: classSlots, traitSlots.
	self classLayout slotScope: newSlotScope.
	self setFormat: self classLayout format
]

{ #category : '*Fuel-Core' }
ClassDescription >> instanceVariableNamesDo: anUnaryBlock [
	"This is part of the interface between the compiler and a class's instance or field names.
	 The class should enumerate anUnaryBlock with the instance variable name strings.  The order is important. Names evaluated later will override the
	 same names occurring earlier."

	<reflection: 'Class structural inspection - Instance variable inspection'>
	| superInstSize |
	superInstSize := self superclass
		ifNotNil: [ self superclass instSize ]
      ifNil: [ 0 ].
	superInstSize > 0 ifTrue: [
		self superclass instanceVariableNamesDo: anUnaryBlock ].
	
	1
		to: self instSize - superInstSize
		do: [ :i |
			anUnaryBlock value: (self instVarNames at: i) ]
]
