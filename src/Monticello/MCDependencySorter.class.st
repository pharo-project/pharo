"
A MCDependencySorter computes the dependencies to a set of entities.
"
Class {
	#name : 'MCDependencySorter',
	#superclass : 'Object',
	#instVars : [
		'required',
		'provided',
		'orderedItems'
	],
	#category : 'Monticello-Loading',
	#package : 'Monticello',
	#tag : 'Loading'
}

{ #category : 'instance creation' }
MCDependencySorter class >> items: aCollection [
	^ self new addAll: aCollection
]

{ #category : 'public' }
MCDependencySorter class >> sortItems: aCollection [
	| sorter |
	sorter := self items: aCollection.
	sorter externalRequirements do: [:req  | sorter addProvision: req].
	^ sorter orderedItems.
]

{ #category : 'building' }
MCDependencySorter >> add: anItem [

	(self unresolvedRequirementsFor: anItem)
		ifEmpty: [ self addToOrder: anItem ]
		ifNotEmpty: [ :requirements | self addRequirements: requirements for: anItem ].
	^ anItem
]

{ #category : 'building' }
MCDependencySorter >> addAll: aCollection [
	aCollection asArray sort do: [:ea | self add: ea]
]

{ #category : 'building' }
MCDependencySorter >> addExternalProvisions: aCollection [
	(aCollection intersection: self externalRequirements)
		do: [:ea | self addProvision: ea]
]

{ #category : 'private' }
MCDependencySorter >> addProvision: anObject [
	| newlySatisfied |
	provided add: anObject.
	newlySatisfied := required removeKey: anObject ifAbsent: [#()].
	self addAll: newlySatisfied.
]

{ #category : 'private' }
MCDependencySorter >> addRequirement: reqObject for: itemObject [
	(self itemsRequiring: reqObject) add: itemObject
]

{ #category : 'private' }
MCDependencySorter >> addRequirements: aCollection for: anObject [
	aCollection do: [:ea | self addRequirement: ea for: anObject]
]

{ #category : 'private' }
MCDependencySorter >> addToOrder: anItem [
	orderedItems add: anItem.
	anItem provisions do: [:ea | self addProvision: ea].
]

{ #category : 'accessing' }
MCDependencySorter >> externalRequirements [
	| unloaded providedByUnloaded |
	unloaded := self itemsWithMissingRequirements.
	providedByUnloaded := (unloaded flatCollect: [:e | e provisions]) asSet.
	^ required keys reject: [:ea | providedByUnloaded includes: ea ]
]

{ #category : 'initialization' }
MCDependencySorter >> initialize [
	super initialize.
	provided := Set new.
	required := Dictionary new.
	orderedItems := OrderedCollection new.
]

{ #category : 'private' }
MCDependencySorter >> itemsRequiring: anObject [
	^ required at: anObject ifAbsentPut: [Set new]
]

{ #category : 'accessing' }
MCDependencySorter >> itemsWithMissingRequirements [
	| items |
	items := Set new.
	required do: [:ea | items addAll: ea].
	^ items

]

{ #category : 'sorting' }
MCDependencySorter >> orderedItems [
	^ orderedItems
]

{ #category : 'private' }
MCDependencySorter >> unresolvedRequirementsFor: anItem [
	^ anItem requirements difference: provided
]
