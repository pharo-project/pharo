"
I represent a container of method extensions read from an Iceberg repository, either from a commit or from the working directory in disk.
The package containing this definition will have as many instances of myself as different classes are extended.

My contents is just an empty string as the extended class/trait is not defined in the package.

See my superclasses for more information.
"
Class {
	#name : #IceExtensionDefinition,
	#superclass : #IceMethodContainerDefinition,
	#category : #'Iceberg-Changes'
}

{ #category : #comparing }
IceExtensionDefinition >> = otherExtension [
	^ self species = otherExtension species 
		and: [ self name = otherExtension name ]
]

{ #category : #visiting }
IceExtensionDefinition >> accept: aVisitor [

	^ aVisitor visitExtensionDefinition: self
]

{ #category : #patching }
IceExtensionDefinition >> addModification: anIceModification toPatcher: aMCPatcher [ 
	
	"Do nothing. Monticello does not have definitions for extensions"
]

{ #category : #patching }
IceExtensionDefinition >> addToPatcher: aMCPatcher [ 
	
	"Monticello does not reify extensions"
]

{ #category : #visiting }
IceExtensionDefinition >> contents [

	^ ''
]

{ #category : #comparing }
IceExtensionDefinition >> hash [
	^ self name hash
]

{ #category : #testing }
IceExtensionDefinition >> isExtensionDefinition [
	
	^ true
]

{ #category : #patching }
IceExtensionDefinition >> removeFrom: aPatcher [
	
	"Do nothing. Do not remove extensions"
]

{ #category : #patching }
IceExtensionDefinition >> removeFromPatcher: aMCPatcher [ 
	
	"Monticello does not reify extensions"
]
