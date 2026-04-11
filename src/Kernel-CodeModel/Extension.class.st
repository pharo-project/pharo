Class {
	#name : 'Extension',
	#superclass : 'Class',
	#instVars : [
		'extendedClass',
		'userPackages'
	],
	#category : 'Kernel-CodeModel-Extensions',
	#package : 'Kernel-CodeModel',
	#tag : 'Extensions'
}

{ #category : 'accessing' }
Extension >> addUserPackage: aPackage [

	(self userPackages includes: aPackage) ifTrue: [ self error: 'Package already extended: ' , aPackage name ].

	self userPackages add: aPackage
]

{ #category : 'accessing' }
Extension >> defineScopedSelector: aSymbol [

	^ self package defineScopedSelector: aSymbol
]

{ #category : 'converting' }
Extension >> definedMethods [
	"Returns the methods of classes excluding the ones of the traits or extensions"

	^ self localMethods
]

{ #category : 'accessing' }
Extension >> extendedClass [

	^ extendedClass
]

{ #category : 'accessing' }
Extension >> extendedClass: anObject [

	extendedClass := anObject
]

{ #category : 'initialization' }
Extension >> hasUserPackages [

	^ userPackages notNil and: [ userPackages notEmpty ]
]

{ #category : 'accessing' }
Extension >> internScopedSelector: aSymbol [

	^ self package internScopedSelector: aSymbol
]

{ #category : 'organization' }
Extension >> methodInstaller [

	^ OCExtensionMethodInstaller new
]

{ #category : 'initialization' }
Extension >> removeFromSystem: logged [

	self selectors do: [ :e |
		self removeSelector: e ].
	super removeFromSystem: logged
]

{ #category : 'accessing - method dictionary' }
Extension >> removeSelector: aSelector [

	super removeSelector: aSelector.
	
	"In some tests we removed the extension method by hand, breaking some invariants.
	We should take a look a that"
	(extendedClass includesSelector: aSelector)
		ifTrue: [ extendedClass removeMethod: (extendedClass >> aSelector) ]
]

{ #category : 'accessing' }
Extension >> removeUserPackage: aPackage [

	self userPackages remove: aPackage
]

{ #category : 'accessing' }
Extension >> scopedSelectors [

	"An extension only has scoped selectors"
	^ self selectors
]

{ #category : 'initialization' }
Extension >> userPackages [

	^ userPackages ifNil: [ userPackages := IdentitySet new ].
]
