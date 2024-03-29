"
I am a null object (Null Object Design Pattern) representing the fact that a class can have no tag (unpackaged classes).
"
Class {
	#name : 'UndefinedPackageTag',
	#superclass : 'PackageTag',
	#category : 'Kernel-CodeModel-Packages',
	#package : 'Kernel-CodeModel',
	#tag : 'Packages'
}

{ #category : 'instance creation' }
UndefinedPackageTag class >> undefinedPackage: aPackage [

	^ self new
		  package: aPackage;
		  yourself
]

{ #category : 'accessing' }
UndefinedPackageTag class >> undefinedPackageTagName [

	^ #Unclassified
]

{ #category : 'accessing' }
UndefinedPackageTag >> categoryName [

	^ #Unclassified
]

{ #category : 'testing' }
UndefinedPackageTag >> isRoot [

	^ true
]

{ #category : 'testing' }
UndefinedPackageTag >> isUndefined [

	^ true
]

{ #category : 'accessing' }
UndefinedPackageTag >> name [

	^ self class undefinedPackageTagName
]

{ #category : 'converting' }
UndefinedPackageTag >> promoteAsPackage [

	self error: 'Cannot promote the undefined package tag'
]

{ #category : 'accessing' }
UndefinedPackageTag >> removeFromSystem [
	"Cannot remove the undefined package tag"

	
]
