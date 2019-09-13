"
A namespace is a container that in addition to managing classes and methods, it also knows how to treat pools and global variables
"
Class {
	#name : #RGNamespace,
	#superclass : #RGContainer,
	#category : #'Ring-Definitions-Containers-Base'
}

{ #category : #adding }
RGNamespace >> addElement: anObject in: aCollection [

	anObject environment: self.
	super addElement: anObject in: aCollection
]

{ #category : #adding }
RGNamespace >> addGlobalVariable: anORGlobalVariableDefinition [
	"adds anORGlobalVariableDefinition to the globals dictionary if satisfies it #isGlobalVariable"

	anORGlobalVariableDefinition isGlobalVariable ifFalse:[ ^self ].
	self addElement: anORGlobalVariableDefinition in: self globalVariables
]

{ #category : #adding }
RGNamespace >> addGlobalVariableNamed: globalVariableName [
	"creates a RGGlobalVariableDefinition with the given name.
	It's verified that a pool with that name does not exist"

	(self includesGlobalVariableNamed: globalVariableName) ifTrue: [ ^self ].
	self addGlobalVariable: (RGGlobalVariableDefinition named: globalVariableName).
]

{ #category : #adding }
RGNamespace >> addPool: anORPoolDefinition [
	"adds anORPoolDefinition to the pools dictionary if satisfies it #isPool"

	anORPoolDefinition isPool ifFalse:[ ^self ].
	self addElement: anORPoolDefinition in: self pools
]

{ #category : #adding }
RGNamespace >> addPoolNamed: poolName [
	"creates a RGPoolDefinition with the given name.
	It's verified that a pool with that name does not exist"
	| pool |

	(self includesPoolNamed: poolName) ifTrue: [ ^self ].
	"A shared pool is a class inheriting from #SharedPool"
	pool := (RGClassDefinition named: poolName)
			superclassName: #SharedPool;
			isPool: true;
			yourself.
	self addPool: pool.
]

{ #category : #'accessing global variables' }
RGNamespace >> globalVariableNamed: globalVariableName [

	^self elementNamed: globalVariableName in: self globalVariables
]

{ #category : #'accessing global variables' }
RGNamespace >> globalVariables [
	"retrieves global variables"

	^self elementsCategorized: #globalVariables
]

{ #category : #'accessing global variables' }
RGNamespace >> globalVariables: aCollection [

	self elementsCategorized: #globalVariables with: aCollection
]

{ #category : #testing }
RGNamespace >> includesGlobalVariable: anORGlobalVariableDefinition [

	^self globalVariables includes: anORGlobalVariableDefinition
]

{ #category : #testing }
RGNamespace >> includesGlobalVariableNamed: globalVariableName [
	
	^self includesElementNamed: globalVariableName in: self globalVariables
]

{ #category : #testing }
RGNamespace >> includesPool: anORPoolDefinition [

	^self pools includes: anORPoolDefinition
]

{ #category : #testing }
RGNamespace >> includesPoolNamed: poolName [
	
	^self includesElementNamed: poolName in: self pools
]

{ #category : #initialization }
RGNamespace >> initialize [

	super initialize.
	self globalVariables: IdentityDictionary new.
	self pools: IdentityDictionary new.
]

{ #category : #testing }
RGNamespace >> isNamespace [

	^true
]

{ #category : #'accessing pools' }
RGNamespace >> poolNamed: poolName [

	^self elementNamed: poolName in: self pools
]

{ #category : #'accessing pools' }
RGNamespace >> pools [
	"retrieves the shares pools"

	^self elementsCategorized: #pools
]

{ #category : #'accessing pools' }
RGNamespace >> pools: aCollection [

	self elementsCategorized: #pools with: aCollection
]

{ #category : #removing }
RGNamespace >> removeGlobalVariable: anORGlobalVariableDefinition [
	"removes anORGlobalVariableDefinition from the globals dictionary if satisfies it #isGlobalVariable"

	anORGlobalVariableDefinition isGlobalVariable ifFalse:[ ^self ].
	self removeElement: anORGlobalVariableDefinition from: self globalVariables
]

{ #category : #removing }
RGNamespace >> removePool: anORPoolDefinition [
	"removes anORPoolDefinition from the pools dictionary if satisfies it #isPool"

	anORPoolDefinition isPool ifFalse:[ ^self ].
	self removeElement: anORPoolDefinition from: self pools
]

{ #category : #removing }
RGNamespace >> withoutGlobalVariables [

	self removeElementsCategorized: #globalVariables
]

{ #category : #removing }
RGNamespace >> withoutPools [

	self removeElementsCategorized: #pools
]
