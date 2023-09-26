"
I am a null object (Null Object Design Pattern) representing the fact that a class can have no tag (unpackaged classes).
"
Class {
	#name : 'UndefinedPackageTag',
	#superclass : 'RPackageTag',
	#category : 'RPackage-Core-Base',
	#package : 'RPackage-Core',
	#tag : 'Base'
}

{ #category : 'accessing' }
UndefinedPackageTag class >> undefinedPackageTagName [

	^ #Unclassified
]

{ #category : 'testing' }
UndefinedPackageTag >> isUndefined [

	^ true
]

{ #category : 'accessing' }
UndefinedPackageTag >> name [
	^ self class undefinedPackageTagName
]
