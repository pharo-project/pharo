"
For the moment not doing much... but having a tree for class definitions is inviting to have a nice visitor. 
"
Class {
	#name : #CDClassDefinitionVisitor,
	#superclass : #Object,
	#instVars : [
		'stream'
	],
	#category : #'FluidClassDefinition-Visitor'
}

{ #category : #visiting }
CDClassDefinitionVisitor >> visitPackage: aPackageNode [
]

{ #category : #visiting }
CDClassDefinitionVisitor >> visitTag: aTagNode [
]
