"
A MCThreeWayMerger is more advanced merge operation.
Operations are subclasses of MCPatchOperation:  addition (MCAddition), modification of an entity (MCModification ) and removal (MCRemoval).

"
Class {
	#name : #MCThreeWayMerger,
	#superclass : #MCMerger,
	#instVars : [
		'index',
		'operations',
		'provisions',
		'redundantAdds'
	],
	#category : 'Monticello-Merging'
}

{ #category : #instance-creation }
MCThreeWayMerger class >> base: aSnapshot patch: aPatch [
	aPatch isEmpty ifTrue: [MCNoChangesException signal].
	^ self new
		addBaseSnapshot: aSnapshot;
		applyPatch: aPatch;
		yourself
		
]

{ #category : #instance-creation }
MCThreeWayMerger class >> base: aSnapshot target: targetSnapshot ancestor: ancestorSnapshot [
	^ self base: aSnapshot patch: (targetSnapshot patchRelativeToBase: ancestorSnapshot)
]

{ #category : #operations }
MCThreeWayMerger >> addBaseSnapshot: aSnapshot [
	aSnapshot definitions do:
		[:ea |
		index add: ea.
		provisions addAll: ea provisions]
]

{ #category : #operations }
MCThreeWayMerger >> addDefinition: aDefinition [
	index
		definitionLike: aDefinition
		ifPresent: [:other |
			(self removalForDefinition: aDefinition)
				ifNotNil:
					[:op |
					self addOperation: (MCModification of: other to: aDefinition).
					self removeOperation: op.
					^ self].
			other = aDefinition
				ifFalse: [self addConflictWithOperation: (MCModification of: other to: aDefinition)]
				ifTrue: [self redundantAdds add: aDefinition]]
		ifAbsent: [self addOperation: (MCAddition of: aDefinition)]
]

{ #category : #operations }
MCThreeWayMerger >> addOperation: anOperation [
	self operations add: anOperation
]

{ #category : #operations }
MCThreeWayMerger >> applyPatch: aPatch [
	aPatch applyTo: self
]

{ #category : #operations }
MCThreeWayMerger >> baseSnapshot [
	^ (MCSnapshot fromDefinitions: index definitions)
]

{ #category : #initialization }
MCThreeWayMerger >> initialize [
	super initialize.
	index := MCDefinitionIndex new.
	provisions := Set new
]

{ #category : #operations }
MCThreeWayMerger >> modificationConflictForDefinition: aDefinition [
	^ self conflicts 
		detect: [:ea |
			(ea definition isRevisionOf: aDefinition) and:
				[ea operation isModification]] 
		ifNone: []
]

{ #category : #operations }
MCThreeWayMerger >> modifyDefinition: baseDefinition to: targetDefinition [
	index
		definitionLike: baseDefinition
		ifPresent: [:other | 
			other = baseDefinition
				ifTrue: [self addOperation: (MCModification of:  baseDefinition to: targetDefinition)]
				ifFalse: [other = targetDefinition
							ifFalse: [self addConflictWithOperation:
											(MCModification of: other to: targetDefinition)]]]
		ifAbsent: [self addConflictWithOperation: (MCAddition of: targetDefinition)]
]

{ #category : #accessing }
MCThreeWayMerger >> operations [
	^ operations ifNil: [operations := OrderedCollection new]
]

{ #category : #accessing }
MCThreeWayMerger >> provisions [
	^ provisions
]

{ #category : #operations }
MCThreeWayMerger >> redundantAdds [
	^ redundantAdds ifNil: [redundantAdds := Set new]
]

{ #category : #operations }
MCThreeWayMerger >> removalForDefinition: aDefinition [
	^ operations ifNotNil:
		[operations
			detect: [:ea | (ea definition isRevisionOf: aDefinition) and: [ea isRemoval]]
			ifNone: []]
]

{ #category : #operations }
MCThreeWayMerger >> removeDefinition: aDefinition [
	index
		definitionLike: aDefinition
		ifPresent: [:other | 
			other = aDefinition
				ifTrue: [(self modificationConflictForDefinition: aDefinition)
							ifNotNil: [:c |
									c beNonConflict.
									^ self]. 
						(self redundantAdds includes: aDefinition)
							ifFalse: [self addOperation: (MCRemoval of: aDefinition)]]
				ifFalse: [self addConflictWithOperation: (MCRemoval of: other)]]
		ifAbsent: []
]

{ #category : #operations }
MCThreeWayMerger >> removeOperation: anOperation [
	operations remove: anOperation
]
