Class {
	#name : 'DependentsManager',
	#superclass : 'Object',
	#instVars : [
		'dependents'
	],
	#classInstVars : [
		'uniqueInstance'
	],
	#category : 'System-Model-Base',
	#package : 'System-Model',
	#tag : 'Base'
}

{ #category : 'adding' }
DependentsManager class >> addDependent: dependent to: anObject [

	^ self uniqueInstance addDependent: dependent to: anObject
]

{ #category : 'adding' }
DependentsManager class >> canDiscardEditsTo: anObject [
	"Answer true if none of the views on this model has unaccepted edits that matter."

	^ self uniqueInstance canDiscardEditsTo: anObject
]

{ #category : 'accessing' }
DependentsManager class >> dependentsOf: anObject [

	^ self uniqueInstance dependentsOf: anObject
]

{ #category : 'accessing' }
DependentsManager class >> fixDependents: copiedReferences [

	self uniqueInstance fixDependents: copiedReferences
]

{ #category : 'adding' }
DependentsManager class >> removeDependent: dependent of: anObject [

	^ self uniqueInstance removeDependent: dependent of: anObject
]

{ #category : 'accessing' }
DependentsManager class >> setDependentsOf: anObject to: aCollectionOrNil [

	^ self uniqueInstance setDependentsOf: anObject to: aCollectionOrNil
]

{ #category : 'accessing' }
DependentsManager class >> uniqueInstance [

	^ uniqueInstance ifNil: [ uniqueInstance := self new ]
]

{ #category : 'initialization' }
DependentsManager >> addDependent: dependent to: anObject [
	"Remove the given object as one of the dependents."

	| theDependents |
	theDependents := (self dependentsOf: anObject).
	(theDependents includes: dependent) ifFalse:
		[self setDependentsOf: anObject to: (theDependents copyWithDependent: dependent)].
	^ dependent
]

{ #category : 'testing' }
DependentsManager >> canDiscardEditsTo: anObject [
	"Answer true if none of the views on this model has unaccepted edits that matter."

	(self dependentsOf: anObject)
		do: [ :each | each canDiscardEdits ifFalse: [ ^ false ] ]
		without: anObject.
	^ true
]

{ #category : 'accessing' }
DependentsManager >> dependentsOf: anObject [

	^ dependents at: anObject ifAbsent: [ #(  ) ]
]

{ #category : 'as yet unclassified' }
DependentsManager >> fixDependents: copiedReferences [

	dependents associationsDo: [ :pair |
		pair value do: [ :dep |
			(copiedReferences at: dep ifAbsent: [ nil ]) ifNotNil: [ :newDep |
				| newModel |
				newModel := copiedReferences at: pair key ifAbsent: [ pair key ].
				newModel addDependent: newDep ] ] ]
]

{ #category : 'initialization' }
DependentsManager >> initialize [

	dependents := WeakIdentityKeyDictionary new
]

{ #category : 'initialization' }
DependentsManager >> removeDependent: dependent of: anObject [
	"Remove the given object as one of the dependents."

	| theDependents |
	theDependents := (self dependentsOf: anObject) reject: [ :each | each == dependent ].
	self setDependentsOf: anObject to: theDependents.
	^ dependent
]

{ #category : 'initialization' }
DependentsManager >> setDependentsOf: anObject to: aCollectionOrNil [
	"Private. Set (or remove) the receiver's dependents list."

	aCollectionOrNil
		ifNil: [ dependents removeKey: anObject ifAbsent: [  ] ]
		ifNotNil: [ dependents at: anObject put: aCollectionOrNil ]
]
