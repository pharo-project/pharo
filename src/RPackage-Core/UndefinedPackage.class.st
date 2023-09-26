"
I am a null object (Null Object Design Pattern) representing the fact that a class can have no package.
"
Class {
	#name : 'UndefinedPackage',
	#superclass : 'RPackage',
	#category : 'RPackage-Core-Base',
	#package : 'RPackage-Core',
	#tag : 'Base'
}

{ #category : 'accessing' }
UndefinedPackage class >> undefinedPackageName [

	^ #_UnpackagedPackage
]

{ #category : 'testing' }
UndefinedPackage >> isUndefined [

	^ true
]

{ #category : 'accessing' }
UndefinedPackage >> name [

	^ RPackage defaultPackageName
]
