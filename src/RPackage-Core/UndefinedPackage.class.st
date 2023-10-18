"
I am a null object (Null Object Design Pattern) representing the fact that a class can have no package.
"
Class {
	#name : 'UndefinedPackage',
	#superclass : 'RPackage',
	#instVars : [
		'undefinedTag'
	],
	#category : 'RPackage-Core-Base',
	#package : 'RPackage-Core',
	#tag : 'Base'
}

{ #category : 'accessing' }
UndefinedPackage class >> undefinedPackageName [

	^ #_UnpackagedPackage
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
