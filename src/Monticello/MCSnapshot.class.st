"
MCSnapshot represents a list of entities compared to a given base entities. 

It holds a list of definitions (instances of MCDefinition subclasses). It can install these entities or update a package.
"
Class {
	#name : #MCSnapshot,
	#superclass : #Object,
	#instVars : [
		'definitions',
		'classDefinitionCache'
	],
	#category : #'Monticello-Base'
}

{ #category : #'instance creation' }
MCSnapshot class >> empty [
	^ self fromDefinitions: #()
]

{ #category : #'instance creation' }
MCSnapshot class >> fromDefinitions: aCollection [
	^ self new initializeWithDefinitions: aCollection
]

{ #category : #comparing }
MCSnapshot >> = other [
	self == other
		ifTrue: [ ^ true ].
	self species = other species
		ifFalse: [ ^ false ].
	^ definitions asArray = other definitions asArray
]

{ #category : #accessing }
MCSnapshot >> classDefinitionNamed: aName ifAbsent: aBlock [

	^ classDefinitionCache at: aName ifAbsent: aBlock
]

{ #category : #accessing }
MCSnapshot >> definitions [
	^ definitions
]

{ #category : #comparing }
MCSnapshot >> hash [
	^ definitions asArray hash
]

{ #category : #initializing }
MCSnapshot >> initializeWithDefinitions: aCollection [
	
	definitions := aCollection.
	classDefinitionCache := Dictionary new.
	definitions do: [ :e |
		e isClassDefinition ifTrue: [ 
			classDefinitionCache
				at: e className
				put: e ] ]
]

{ #category : #loading }
MCSnapshot >> install [
	MCPackageLoader installSnapshot: self
]

{ #category : #patching }
MCSnapshot >> patchRelativeToBase: aSnapshot [
	^ MCPatch fromBase: aSnapshot target: self
]

{ #category : #loading }
MCSnapshot >> updatePackage: aPackage [
	MCPackageLoader updatePackage: aPackage withSnapshot: self
]
