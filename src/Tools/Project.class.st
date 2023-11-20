"
I am an abstraction to represent a project in Pharo.

Since we do not have yet a real project reification we are defining projects and the content of a baseline
"
Class {
	#name : 'Project',
	#superclass : 'Object',
	#instVars : [
		'baseline'
	],
	#category : 'Tools',
	#package : 'Tools'
}

{ #category : 'instance creation' }
Project class >> baseline: aClass [

	^ self new
		  baseline: aClass;
		  yourself
]

{ #category : 'comparing' }
Project >> = anObject [

	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	^ self baseline = anObject baseline
]

{ #category : 'accessing' }
Project >> baseline [
	^ baseline
]

{ #category : 'accessing' }
Project >> baseline: anObject [
	baseline := anObject
]

{ #category : 'testing' }
Project >> hasPackages [

	^ self packages isNotEmpty
]

{ #category : 'comparing' }
Project >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ baseline hash
]

{ #category : 'testing' }
Project >> includesPackage: aPackage [

	^ self packages includes: aPackage
]

{ #category : 'accessing' }
Project >> name [

	^ self baseline name withoutPrefix: #BaselineOf
]

{ #category : 'accessing' }
Project >> packageManager [

	^ self baseline packageOrganizer
]

{ #category : 'accessing' }
Project >> packages [

	^ self baseline withAllPackageNames
		  select: [ :packageName | self packageManager hasPackage: packageName ]
		  thenCollect: [ :packageName | self packageManager packageNamed: packageName ]
]
