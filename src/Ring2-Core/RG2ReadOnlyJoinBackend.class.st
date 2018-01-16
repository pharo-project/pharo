Class {
	#name : #RG2ReadOnlyJoinBackend,
	#superclass : #RG2ReadOnlyBackend,
	#instVars : [
		'environment1',
		'environment2'
	],
	#category : #'Ring2-Core-Environment'
}

{ #category : #'as yet unclassified' }
RG2ReadOnlyJoinBackend >> behaviorsFor: aRG2Environment do: aBlock [

	| enumerated  |
	
	enumerated := IdentitySet new. 
	
	self environment1 behaviorsDo: [ :each |
		| corresponding | 
		corresponding := each correspondingObjectIn: environment2.
		
		enumerated add: each.
		corresponding 
			ifNil: [	aBlock value: (self copyFor: each) ]
			ifNotNil: [ 
				aBlock value: (self copyFor: (self chooseFrom: each and: corresponding)).
				enumerated add: corresponding ]].
				
	self environment2 behaviorsDo: [ :each | 
		(enumerated includes: each) 
			ifFalse: [ aBlock value: (self copyFor: each) ] ].
]

{ #category : #'as yet unclassified' }
RG2ReadOnlyJoinBackend >> chooseFrom: aRG2Definition1 and: aRG2Definition2 [

	| firstResolved |
	
	"(aRG2Definition1 canMergeSimplyWithoutName: aRG2Definition2)
		ifFalse: [ RG2JoiningError signal]."

	(aRG2Definition1 canMergeWith:	aRG2Definition2 visited: IdentitySet new)	
		ifFalse: [ RG2JoiningError signal].
	"
	(aRG2Definition1 canBeJoined not and: [ aRG2Definition2 canBeJoined not ])
		ifTrue: [ RG2JoiningError signal ].
	"
	firstResolved := aRG2Definition1 isRing2Resolved.
		
	^ firstResolved 
		ifTrue: [ aRG2Definition1 ]
		ifFalse: [ aRG2Definition2 ].
]

{ #category : #'as yet unclassified' }
RG2ReadOnlyJoinBackend >> copyFor: aRG2Definition [

	^ self definitionFor: aRG2Definition ifAbsentRegister: [
		aRG2Definition copyForEnvironment: self environment].
]

{ #category : #accessing }
RG2ReadOnlyJoinBackend >> environment1 [
	^ environment1
]

{ #category : #accessing }
RG2ReadOnlyJoinBackend >> environment1: anObject [
	environment1 := anObject
]

{ #category : #accessing }
RG2ReadOnlyJoinBackend >> environment2 [
	^ environment2
]

{ #category : #accessing }
RG2ReadOnlyJoinBackend >> environment2: anObject [
	environment2 := anObject
]

{ #category : #'as yet unclassified' }
RG2ReadOnlyJoinBackend >> initialize [

	super initialize.
	
	providedDefinitions := IdentityDictionary new.
	
]

{ #category : #behavior }
RG2ReadOnlyJoinBackend >> layoutFor: aRG2BehaviorDefinition [

	^ self operation: [ :backend :definition |
			backend layoutFor: definition ]
		on: aRG2BehaviorDefinition
	
]

{ #category : #behavior }
RG2ReadOnlyJoinBackend >> metaclassFor: aRG2BehaviorDefinition [

	^ self operation: [ :backend :definition |
			backend metaclassFor: definition ]
		on: aRG2BehaviorDefinition
	
]

{ #category : #behavior }
RG2ReadOnlyJoinBackend >> operation: aBlock on: aRG2BehaviorDefinition [

	"aBlock accepts backend and definition as arguments"

	| originalDefinition originalEnvironment otherEnvironment result1 result2 |

	originalDefinition := aRG2BehaviorDefinition propertyNamed: #originalDefinition.
	originalEnvironment := originalDefinition environment.
	otherEnvironment := (originalEnvironment == environment1)
		ifTrue: [ environment2  ]
		ifFalse: [ environment1 ].
		
	result1 := aBlock value: originalEnvironment backend value: originalDefinition.
	result2 := result1 correspondingObjectIn: otherEnvironment.
	
	result2 ifNil: [ ^ self copyFor: result1 ].
	
	^ self copyFor: (self chooseFrom: result1 and: result2).
	

	
	
	

]

{ #category : #environment }
RG2ReadOnlyJoinBackend >> packagesFor: aRG2Environment do: aBlock [

	| enumerated |
	
	enumerated := IdentitySet new. 
	
	self environment1 packagesDo: [ :each |
		| corresponding | 
		corresponding := each correspondingObjectIn: environment2.
		
		enumerated add: each.
		corresponding 
			ifNil: [	aBlock value: (self copyFor: each) ]
			ifNotNil: [ 
				aBlock value: (self copyFor: (self chooseFrom: each and: corresponding)).
				enumerated add: corresponding ]].
				
	self environment2 packagesDo: [ :each | 
		(enumerated includes: each) 
			ifFalse: [ aBlock value: (self copyFor: each) ] ].
]

{ #category : #behavior }
RG2ReadOnlyJoinBackend >> superclassFor: aRG2BehaviorDefinition [

	^ self operation: [ :backend :definition |
			backend superclassFor: definition ]
		on: aRG2BehaviorDefinition	

	
	
	

]

{ #category : #behavior }
RG2ReadOnlyJoinBackend >> traitCompositionFor: aRG2BehaviorDefinition [

	^ self operation: [ :backend :definition |
			backend traitCompositionFor: definition ]
		on: aRG2BehaviorDefinition
	
]
