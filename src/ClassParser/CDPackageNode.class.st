"
I'm a simple node to represent the package part in a class definition.
"
Class {
	#name : 'CDPackageNode',
	#superclass : 'CDNode',
	#instVars : [
		'packageName'
	],
	#category : 'ClassParser-Model',
	#package : 'ClassParser',
	#tag : 'Model'
}

{ #category : 'accessing' }
CDPackageNode >> packageName [
	^ packageName
]

{ #category : 'accessing' }
CDPackageNode >> packageName: anObject [
	packageName := anObject
]

{ #category : 'accessing' }
CDPackageNode >> value [
	"to be compatible with the default CDClassDefinitionNode definition.
	There a package is just an ASTLiteralNode and it may be a PackageNode too."

	^ packageName
]
