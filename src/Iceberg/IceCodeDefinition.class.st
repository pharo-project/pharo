"
I am an IceDefinition that have some source code in my contents.

See my sub/superclasses for more information.
"
Class {
	#name : #IceCodeDefinition,
	#superclass : #IceDefinition,
	#category : #'Iceberg-Changes'
}

{ #category : #'as yet unclassified' }
IceCodeDefinition class >> named: aName source: aString [
	
	^ (super named: aName)
		source: aString;
		yourself
]

{ #category : #comparing }
IceCodeDefinition >> = anotherDefinition [

	^ super = anotherDefinition
		and: [ self contents = anotherDefinition contents ]
]

{ #category : #patching }
IceCodeDefinition >> addModification: anIceModification toPatcher: aMCPatcher [ 
	
	aMCPatcher
		modifyDefinition: anIceModification rightDefinition asMCDefinition
		to: anIceModification leftDefinition asMCDefinition
]

{ #category : #patching }
IceCodeDefinition >> addToPatcher: aMCPatcher [ 
	
	self subclassResponsibility
]

{ #category : #accessing }
IceCodeDefinition >> contents [
	
	^ self subclassResponsibility
]

{ #category : #comparing }
IceCodeDefinition >> hash [

	^ self contents hash
]

{ #category : #patching }
IceCodeDefinition >> removeFrom: aPatcher [
	
	self subclassResponsibility
]

{ #category : #patching }
IceCodeDefinition >> removeFromPatcher: aMCPatcher [ 
	
	self subclassResponsibility
]
