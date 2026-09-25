Class {
	#name : 'Extension',
	#superclass : 'Class',
	#instVars : [
		'extendedClass',
		'userPackages'
	],
	#classVars : [
		'AllowMigration'
	],
	#category : 'Kernel-CodeModel-Extensions',
	#package : 'Kernel-CodeModel',
	#tag : 'Extensions'
}

{ #category : 'accessing' }
Extension class >> allowMigration [

	^ AllowMigration ifNil: [ true ]
]

{ #category : 'accessing' }
Extension class >> allowMigration: aBoolean [

	^ AllowMigration := aBoolean
]

{ #category : 'accessing' }
Extension >> addUserPackage: aPackage [

	(self userPackages includes: aPackage) ifTrue: [ self error: 'Package already extended: ' , aPackage name ].

	self userPackages add: aPackage
]

{ #category : 'accessing - method dictionary' }
Extension >> compiledMethodAt: selector [

	^ self methodDict at: (self package visibleSelectorForSymbol: selector) 
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

{ #category : 'testing - method dictionary' }
Extension >> includesSelector: aSymbol [

	(super includesSelector: aSymbol) ifTrue: [ ^ true ].

	^ super includesSelector: (self package visibleSelectorForSymbol: aSymbol)
]

{ #category : 'accessing' }
Extension >> internScopedSelector: aSymbol [

	^ self package internScopedSelector: aSymbol
]

{ #category : 'testing' }
Extension >> isExtension [

	^ true
]

{ #category : 'organization' }
Extension >> methodInstaller [

	^ OCExtensionMethodInstaller new
]

{ #category : 'initialization' }
Extension >> removeFromSystem: logged [

	self selectors do: [ :e |
		self removeSelector: e logged: logged ].
	super removeFromSystem: logged
]

{ #category : 'accessing - method dictionary' }
Extension >> removeSelector: aSelector logged: logged [
	"System usually request method removal using a Symbol. However, with the conch system, the actual key in the method dictionary might be a ScopedSelector. 
	We must translate the symbol to its scoped equivalent (if needed) to prevent a KeyNotFound error in the super call"

	| targetSelector |
	targetSelector := (self methodDict includesKey: aSelector)
		                  ifTrue: [ aSelector ]
		                  ifFalse: [ self package visibleSelectorForSymbol: aSelector asSymbol ].

	super removeSelector: targetSelector logged: logged.

	"In some tests we removed the extension method by hand, breaking some invariants.
	We should take a look a that"
	(extendedClass includesSelector: aSelector) ifTrue: [ extendedClass removeMethod: extendedClass >> aSelector ]
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
