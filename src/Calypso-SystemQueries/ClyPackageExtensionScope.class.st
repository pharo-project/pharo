"
I am a scope of packages which shows only objects which packages extend.
I see all extended classes and extension methods
"
Class {
	#name : #ClyPackageExtensionScope,
	#superclass : #ClyTypedScope,
	#category : #'Calypso-SystemQueries-Scopes'
}

{ #category : #queries }
ClyPackageExtensionScope >> classesDo: aBlock [ 
	self packagesDo: [ :package | 
		package extendedClasses 
			collect: [ :each | each instanceSide ] 
			thenDo: aBlock]
]

{ #category : #queries }
ClyPackageExtensionScope >> methodsDo: aBlock [
	
	self packagesDo: [ :package | 
		package extensionMethods do: aBlock ]
]

{ #category : #queries }
ClyPackageExtensionScope >> packagesDo: aBlock [
	basisObjects do: aBlock
]

{ #category : #printing }
ClyPackageExtensionScope >> printBasisObject: aPackage on: aStream [

	aStream nextPutAll: aPackage name
]
