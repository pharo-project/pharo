"
I am a scope of packages.
So I see all objects which are defined by given packages: 
- packages
- classes 
- methods
"
Class {
	#name : #ClyPackageScope,
	#superclass : #ClyTypedScope,
	#category : #'Calypso-SystemQueries-Scopes'
}

{ #category : #accessing }
ClyPackageScope class >> defaultName [
	^'packages'
]

{ #category : #'refactoring support' }
ClyPackageScope >> asRBEnvironment [
	^RBPackageEnvironment packages: basisObjects
]

{ #category : #queries }
ClyPackageScope >> classGroupProvidersDo: aBlock [
	
	| providers |
	environment pluginsDo: [ :plugin |
		providers := plugin collectClassGroupProviders.
		providers do: aBlock]
]

{ #category : #queries }
ClyPackageScope >> classGroupsDo: aBlock [

	self classGroupProvidersDo: [ :groupProvider |
		groupProvider classGroupsIn: self do: [:group | 
			aBlock value: group] ]
]

{ #category : #queries }
ClyPackageScope >> classesDo: aBlock [
	self packagesDo: [ :package | 
		package definedClasses do: aBlock]
]

{ #category : #'system changes' }
ClyPackageScope >> includesClassGroupsAffectedBy: aSystemAnnouncement [

	| providers |
	basisObjects do: [ :eachPackage | 
		environment pluginsDo: [ :plugin |
			providers := plugin collectClassGroupProviders.
			providers do: [ :eachProvider | 
				(eachProvider providesGroupsAffectedBy: aSystemAnnouncement inScope: self)
					ifTrue: [^true]]]].
	^false
]

{ #category : #queries }
ClyPackageScope >> methodsDo: aBlock [
	self classesDo: [:class | 
		class localMethods do: aBlock.
		class classSide localMethods do: aBlock].
	
	self packagesDo: [ :package | 
		package extensionMethods do: aBlock ]
]

{ #category : #queries }
ClyPackageScope >> packagesDo: aBlock [
	basisObjects do: aBlock
]

{ #category : #printing }
ClyPackageScope >> printBasisObject: aPackage on: aStream [

	^aStream nextPutAll: aPackage name
]
