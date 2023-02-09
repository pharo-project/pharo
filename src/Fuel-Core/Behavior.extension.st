Extension { #name : #Behavior }

{ #category : #'*Fuel-Core' }
Behavior >> fuelIgnoredInstanceVariableNames [
	"Indicates which variables have to be ignored during serialization."

	^#()
]

{ #category : #'*Fuel-Core' }
Behavior >> fuelInitializeTraitComposition [

	self traitComposition traits do: [:each | each addUser: self]
]

{ #category : #'*Fuel-Core' }
Behavior >> fuelNew [
	"Answer an instance of mine in which serialized references will be injected."

	^ self basicNew
]

{ #category : #'*Fuel-Core' }
Behavior >> fuelNew: sizeRequested [ 
	"Answer an instance of mine in which serialized references will be injected."

	^ self basicNew: sizeRequested
]

{ #category : #'*Fuel-Core' }
Behavior >> fuelRecomputeSlotScopeWithTraits [
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
	(FLPlatform current hasStatefulTraits not or: [
		self traitComposition slots allSatisfy: [ :slot |
			self hasSlotNamed: slot name ] ]) ifTrue: [ ^ self ].
	
	classSlots := self classLayout slots.
	traitSlots := self traitComposition slots select: [ :traitSlot |
		classSlots noneSatisfy: [ :slot |
			slot name = traitSlot name ] ].
	
	newSlotScope := self classLayout slotScope parentScope extend: classSlots, traitSlots.
	self classLayout slotScope: newSlotScope.
	self setFormat: self classLayout format
]
