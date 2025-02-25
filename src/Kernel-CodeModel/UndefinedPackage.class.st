"
I am a null object (Null Object Design Pattern) representing the fact that a class can have no package.
"
Class {
	#name : 'UndefinedPackage',
	#superclass : 'Package',
	#instVars : [
		'undefinedTag'
	],
	#category : 'Kernel-CodeModel-Packages',
	#package : 'Kernel-CodeModel',
	#tag : 'Packages'
}

{ #category : 'accessing' }
UndefinedPackage class >> undefinedPackageName [

	^ #_UnpackagedPackage
]

{ #category : 'adding' }
UndefinedPackage >> addMethod: aCompiledMethod [
	"We do not import the methods in this package."
]

{ #category : 'class tags' }
UndefinedPackage >> ensureTag: aTag [

	aTag ifNil: [ ^ undefinedTag ].

	^ aTag = undefinedTag
		  ifTrue: [ aTag ]
		  ifFalse: [ self error: 'Cannot add a tag to the UndefinedPackage' ]
]

{ #category : 'initialization' }
UndefinedPackage >> initialize [

	super initialize.
	undefinedTag := UndefinedPackageTag undefinedPackage: self
]

{ #category : 'testing' }
UndefinedPackage >> isEmpty [

	^ false
]

{ #category : 'testing' }
UndefinedPackage >> isUndefined [

	^ true
]

{ #category : 'accessing' }
UndefinedPackage >> name [

	^ #_UnpackagedPackage
]

{ #category : 'removing' }
UndefinedPackage >> removeFromSystem [
	"Cannot remove the undefined package."
]

{ #category : 'class tags' }
UndefinedPackage >> tagOf: aClass [

	^ self undefinedTag
]

{ #category : 'accessing' }
UndefinedPackage >> undefinedTag [

	^ undefinedTag
]
