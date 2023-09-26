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

	^ { undefinedTag }
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
]

{ #category : 'testing' }
UndefinedPackage >> isUndefined [

	^ true
]

{ #category : 'accessing' }
UndefinedPackage >> name [

	^ RPackage defaultPackageName
]

{ #category : 'removing' }
UndefinedPackage >> removeFromSystem [
	"Cannot remove the undefined package."
]
