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

{ #category : 'class tags' }
UndefinedPackage >> classTags [

	^ { undefinedTag } asOrderedCollection
]

{ #category : 'accessing' }
UndefinedPackage >> classes [
	"The undefined package does not keep track of the classes it contains."

	^ #(  )
]

{ #category : 'initialization' }
UndefinedPackage >> initialize [

	super initialize.
	undefinedTag := UndefinedPackageTag new
		                package: self;
		                yourself
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

	^ undefinedTag
]
