"
I represent a modification in a class. 
"
Class {
	#name : #ClassModification,
	#superclass : #AbstractClassModification,
	#instVars : [
		'superclass',
		'additions',
		'removals',
		'changes',
		'slotShift',
		'methodModification',
		'copies',
		'traitComposition',
		'classVariables',
		'sharedPools'
	],
	#category : 'Slot-ClassBuilder'
}

{ #category : #'instance creation' }
ClassModification class >> modify: aClass extend: aSuperclass withLayoutType: layoutClass slots: someSlots sharedVariables: someClassVariables sharedPools: someSharedPools traitComposition: aTraitComposition [ 
	^ self new
		target: aClass;
		superclass: aSuperclass;
		traitComposition: aTraitComposition;
		classVariables: someClassVariables;
		sharedPools: someSharedPools;
		layoutClass: layoutClass slots: someSlots;
		yourself
]

{ #category : #accessing }
ClassModification >> additions [
	^ additions
]

{ #category : #accessing }
ClassModification >> additions: aCollection [
	additions := aCollection
]

{ #category : #private }
ClassModification >> buildModificationMapFor: aModification [
	aModification slotShift: (ShiftedField new shift: slotShift).
	
	aModification size = 0 
		ifTrue: [ ^ aModification ].
	
	additions do: [ :slot | 
		self installSlotAddition: slot on: aModification].
	
	changes keysAndValuesDo: [ :newSlot :oldSlot | 
		self installSlotChangeFrom: oldSlot to: newSlot on: aModification ].
	
	removals do: [ :slot | 
		self installSlotRemoval: slot on: aModification].
	
	copies keysAndValuesDo: [ :newSlot :oldSlot | 
		self installSlotCopiedFrom: oldSlot to: newSlot on: aModification ].
	
	^ aModification
]

{ #category : #accessing }
ClassModification >> changes [
	^ changes
]

{ #category : #accessing }
ClassModification >> classVariables [
	
	^ classVariables
]

{ #category : #accessing }
ClassModification >> classVariables: anObject [
	
	classVariables := anObject
]

{ #category : #private }
ClassModification >> computeChange [
	super computeChange.
	layout computeChangesFrom: target classLayout in: self.
	slotShift := layout fieldSize - target classLayout fieldSize.
	self propagate.
]

{ #category : #accessing }
ClassModification >> copies [
	^ copies
]

{ #category : #initialization }
ClassModification >> initialize [
	super initialize.
	changes := IdentityDictionary new.
	copies := IdentityDictionary new.
	removals := {}.
	additions := {}.
]

{ #category : #installing }
ClassModification >> installSlotAddition: slot on: aModification [
	0 to: slot size - 1 do: [ :idx |
		(AddedField new
			newSlot: slot;
			fieldIndex: idx) installOn: aModification  ]
]

{ #category : #installing }
ClassModification >> installSlotChangeFrom: oldSlot to: newSlot on: aModification [
	0 to: newSlot size - 1 do: [ :idx |
 		(ModifiedField new
			newSlot: newSlot;
			originalSlot: oldSlot;
			fieldIndex: idx) installOn: aModification ]
]

{ #category : #installing }
ClassModification >> installSlotCopiedFrom: oldSlot to: newSlot on: aModification [
	0 to: newSlot size - 1 do: [ :idx |
 		(UnmodifiedField new
			slot: newSlot;
			fieldIndex: idx) installOn: aModification ]
]

{ #category : #installing }
ClassModification >> installSlotRemoval: slot on: aModification [
	0 to: slot size - 1 do: [ :idx |
		(RemovedField new
			originalSlot: slot;
			fieldIndex: idx) installOn: aModification ]
]

{ #category : #accessing }
ClassModification >> instanceModification [
	| map |
	map := InstanceModification new: layout fieldSize.
	^ self buildModificationMapFor: map
]

{ #category : #testing }
ClassModification >> isPropagation [

	^ false
]

{ #category : #accessing }
ClassModification >> layoutClass: layoutClass slots: slotArray [
	| parentLayout newScope newLayout |
	parentLayout := self superLayout.
	newScope := parentLayout slotScope extend: slotArray.
	newLayout := layoutClass extending: parentLayout scope: newScope host: self target.
	self layout: newLayout
]

{ #category : #accessing }
ClassModification >> methodModification [
	^ methodModification ifNil: [
		methodModification := MethodModification new: target classLayout fieldSize.
		self buildModificationMapFor: methodModification ]
]

{ #category : #testing }
ClassModification >> modifiesClassVariables [
	"Note: The meta class has the same class variables as it's non-meta class"

	^ target isInstanceSide and: [ target classVariables ~= classVariables ]
]

{ #category : #testing }
ClassModification >> modifiesFormat [

	^ (layout format = target classLayout format) not
]

{ #category : #testing }
ClassModification >> modifiesSharedPools [

	^ (target sharedPoolsString = sharedPools) not
]

{ #category : #testing }
ClassModification >> modifiesSlots [

	^ (changes isEmpty and: [ removals isEmpty and: [ additions isEmpty ]]) not
]

{ #category : #testing }
ClassModification >> modifiesSuperclass [

	^ (target superclass == superclass) not
]

{ #category : #accessing }
ClassModification >> newScope [
	^ layout slotScope
]

{ #category : #accessing }
ClassModification >> originalScope [
	^ target classLayout slotScope
]

{ #category : #accessing }
ClassModification >> removals [
	^ removals
]

{ #category : #accessing }
ClassModification >> removals: aCollection [
	removals := aCollection
]

{ #category : #accessing }
ClassModification >> sharedPools [
	
	^ sharedPools
]

{ #category : #accessing }
ClassModification >> sharedPools: anObject [
	
	sharedPools := anObject
]

{ #category : #accessing }
ClassModification >> slotShift [
	^ slotShift
]

{ #category : #accessing }
ClassModification >> superLayout [
	^ superclass
		ifNil: [ EmptyLayout instance ]
		ifNotNil: [ superclass classLayout ]
]

{ #category : #accessing }
ClassModification >> superclass [
	^ superclass
]

{ #category : #accessing }
ClassModification >> superclass: aClass [
	superclass := aClass
]

{ #category : #accessing }
ClassModification >> traitComposition [
	^ traitComposition
]

{ #category : #accessing }
ClassModification >> traitComposition: aTraitComposition [
	" We copy the trait composition since we might want "
	" to update the slot indices if the composition is partly stateful "
	traitComposition := aTraitComposition copyTraitExpression
]
