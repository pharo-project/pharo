"
I represent a merge operation. I know how to install myself in the system, how to inverse myself and how to show the definitions I'm being applied to. Check my subclasses for more information.
"
Class {
	#name : #IceOperation,
	#superclass : #Object,
	#instVars : [
		'diff',
		'modifiedNode',
		'definition'
	],
	#category : #'Iceberg-Changes'
}

{ #category : #'instance creation' }
IceOperation class >> definition: aDefinition [
	
	^ self new
		definition: aDefinition;
		yourself
]

{ #category : #comparing }
IceOperation >> = anotherOperation [

	^ self species = anotherOperation species
		and: [ self leftDefinition = anotherOperation leftDefinition ]
]

{ #category : #visiting }
IceOperation >> accept: aVisitor [
	
	self subclassResponsibility
]

{ #category : #'morphic-compatibility' }
IceOperation >> contextClass [
	
	^ definition contextClass
]

{ #category : #accessing }
IceOperation >> definition [
	^ definition
]

{ #category : #accessing }
IceOperation >> definition: aDefinition [

	definition := aDefinition
]

{ #category : #accessing }
IceOperation >> description [
	
	^ definition name
]

{ #category : #testing }
IceOperation >> hasChanges [
	
	^ true
]

{ #category : #comparing }
IceOperation >> hash [
	^ self leftDefinition hash
]

{ #category : #operations }
IceOperation >> install [
	
	self leftDefinition install
]

{ #category : #operations }
IceOperation >> inverse [
	
	self subclassResponsibility
]

{ #category : #testing }
IceOperation >> isAddition [
	
	^ false
]

{ #category : #testing }
IceOperation >> isModification [
	
	^ false
]

{ #category : #testing }
IceOperation >> isRemoval [
	
	^ false
]

{ #category : #testing }
IceOperation >> isResolved [
	
	^ false
]

{ #category : #accessing }
IceOperation >> key [
	
	^ definition key
]

{ #category : #accessing }
IceOperation >> leftContents [
	
	self subclassResponsibility
]

{ #category : #accessing }
IceOperation >> leftDefinition [

	^ definition
]

{ #category : #accessing }
IceOperation >> leftDefinition: aNode [

	definition := aNode
]

{ #category : #merging }
IceOperation >> mergeWithLeftAddition: anIceAddition [ 
	
	self subclassResponsibility
]

{ #category : #merging }
IceOperation >> mergeWithLeftModification: anIceModification [ 
	
	^ self subclassResponsibility
]

{ #category : #merging }
IceOperation >> mergeWithLeftNoOperation: anIceNoModification [ 
	
	^ self subclassResponsibility
]

{ #category : #merging }
IceOperation >> mergeWithLeftRemoval: anIceRemoval [ 
	
	self subclassResponsibility
]

{ #category : #merging }
IceOperation >> mergeWithOperation: anOperation [
	
	self subclassResponsibility
]

{ #category : #accessing }
IceOperation >> name [
	
	^ definition name
]

{ #category : #accessing }
IceOperation >> rightContents [
	
	^ definition contents
]
