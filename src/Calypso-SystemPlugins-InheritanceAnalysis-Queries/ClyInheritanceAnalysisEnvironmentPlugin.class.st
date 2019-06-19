"
I plug environment with many kind of inheritance hints.

I tag abstract classes and method.
I analyze method inheritance and tag method with overriden and overriding tags.

I maintain cache of override method statuses in variable methodCache.
It is a dictionary in form of selector->methodClass->status.
Status is represented by simple two items array in form {ClyOverridingMethodTag instance. ClyOverriddenMethodTag instance}. If method does not overridden then there will be nil in this status array at 2 second position.
I am subscribed on system changes and invalidate methodCache when related classes or methods are changed (look at #attachToSystem method for details).

In addition I provide following method groups:
	- abstract methods
	- override methods
	- overridden methods 
	- should be implemented methods
"
Class {
	#name : #ClyInheritanceAnalysisEnvironmentPlugin,
	#superclass : #ClySystemEnvironmentPlugin,
	#instVars : [
		'methodCache',
		'cacheGuard'
	],
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries'
}

{ #category : #testing }
ClyInheritanceAnalysisEnvironmentPlugin class >> isSlow [
	^true
]

{ #category : #'methods analysis' }
ClyInheritanceAnalysisEnvironmentPlugin >> analysisCacheFor: aMethod do: aBlock [
	
	cacheGuard critical: [ 
		methodCache at: aMethod selector ifPresent: [:classCache | 
			classCache at: aMethod methodClass ifPresent: aBlock]
	]
]

{ #category : #'methods analysis' }
ClyInheritanceAnalysisEnvironmentPlugin >> analyzeMethod: aMethod [
	| cache classCache |
	cache := Array new: 2.
	aMethod isOverriding ifTrue: [ cache at: 1 put: ClyOverridingMethodTag instance ].
	aMethod isOverridden ifTrue: [ cache at: 2 put: ClyOverriddenMethodTag instance ].
	
	cacheGuard critical: [ 
		classCache := methodCache at: aMethod selector ifAbsentPut: [WeakIdentityKeyDictionary new].
		classCache at: aMethod methodClass put: cache]
]

{ #category : #controlling }
ClyInheritanceAnalysisEnvironmentPlugin >> attachToSystem [
	super attachToSystem.
	
	environment system when: MethodAdded, MethodRemoved send: #processMethodChange: to: self.
	environment system when: ClassRemoved send: #processClassRemoval: to: self.
	environment system when: ClassModifiedClassDefinition send: #processClassDefinitionChange: to: self.
	environment system when: ClassModificationApplied send: #processFullClassChange: to: self.
]

{ #category : #'item decoration' }
ClyInheritanceAnalysisEnvironmentPlugin >> checkClassIsAbstract: aClass [
	"Method is copied from Pharo7 Behavior>>isAbstract"
	
	aClass withAllSuperclassesDo: [ :eachClass | 
		eachClass methodsDo: [ :eachMethod |
			(eachMethod isAbstract and: [ (aClass lookupSelector: eachMethod selector) isAbstract ])
				ifTrue: [ ^true ]]].
	
	^false
]

{ #category : #'method groups' }
ClyInheritanceAnalysisEnvironmentPlugin >> collectMethodGroupProviders [
	^{ClyAbstractMethodGroupProvider. ClyOverridingMethodGroupProvider. ClyOverriddenMethodGroupProvider. ClyRequiredMethodGroupProvider}
		collect: [ :each | each new]
]

{ #category : #'item decoration' }
ClyInheritanceAnalysisEnvironmentPlugin >> decorateBrowserItem: anItem ofClass: aClass [
	(aClass classSide includesLocalSelector: #isAbstract) ifTrue: [ 
		aClass instanceSide isAbstract ifTrue: [ anItem markWith: ClyAbstractItemTag ].
		^self].
		
	(self checkClassIsAbstract: aClass) ifTrue: [ 
		anItem markWith: ClyAbstractItemTag ]
]

{ #category : #'item decoration' }
ClyInheritanceAnalysisEnvironmentPlugin >> decorateBrowserItem: anItem ofMethod: aMethod [

	aMethod isAbstract ifTrue: [ anItem markWith: ClyAbstractItemTag ].
	
	(self isMethodOverriding: aMethod) ifTrue: [ 
		anItem markWith: ClyOverridingMethodTag ].
	(self isMethodOverridden: aMethod) ifTrue: [ 
		anItem markWith: ClyOverriddenMethodTag ]
]

{ #category : #controlling }
ClyInheritanceAnalysisEnvironmentPlugin >> detachFromSystem [ 
	environment system unsubscribe: self.
	
	super detachFromSystem
]

{ #category : #'methods analysis' }
ClyInheritanceAnalysisEnvironmentPlugin >> doesClassCache: classCache includesOverridesOf: aClass [

	classCache keysDo: [ :eachClass | 
		((eachClass instanceSide includesBehavior: aClass instanceSide) 
			or: [ aClass instanceSide includesBehavior: eachClass instanceSide ])
				ifTrue: [ ^true ]].
		
	^false
]

{ #category : #initialization }
ClyInheritanceAnalysisEnvironmentPlugin >> initialize [
	super initialize.
	
	cacheGuard := Mutex new.
	methodCache := IdentityDictionary new
]

{ #category : #'methods analysis' }
ClyInheritanceAnalysisEnvironmentPlugin >> isMethodOverridden: aMethod [
	
	self analysisCacheFor: aMethod do: [ :tags | ^tags last notNil].

	self analyzeMethod: aMethod.	
	^self isMethodOverridden: aMethod
]

{ #category : #'methods analysis' }
ClyInheritanceAnalysisEnvironmentPlugin >> isMethodOverriding: aMethod [
	
	self analysisCacheFor: aMethod do: [ :tags | ^tags first notNil].
	
	self analyzeMethod: aMethod.	
	^self isMethodOverriding: aMethod
]

{ #category : #controlling }
ClyInheritanceAnalysisEnvironmentPlugin >> processClassDefinitionChange: aClassDefinitionChange [
	aClassDefinitionChange oldClassDefinition superclass ifNotNil: [:superclass |
		"Two definition changes are always triggered. 
		We always skip the first one when system is inconsistent state 
		which is reflected by following condition"
		(superclass subclasses includes: aClassDefinitionChange oldClassDefinition)
			ifTrue: [ ^self ] ].
	(aClassDefinitionChange oldClassDefinition superclass 
		= aClassDefinitionChange newClassDefinition superclass) ifTrue: [ ^self ].	
	
	self resetCacheOfClass: aClassDefinitionChange oldClassDefinition superclass.
	environment systemChanged: (
		ClyOverriddenSuperclassesChanged overridingSubclass: aClassDefinitionChange oldClassDefinition)
]

{ #category : #controlling }
ClyInheritanceAnalysisEnvironmentPlugin >> processClassRemoval: aClassRemoved [
	self resetCacheOfClass: aClassRemoved classAffected.
	self resetCacheOfClass: aClassRemoved classAffected superclass.
	
	environment systemChanged: (
		ClyOverriddenSuperclassesChanged overridingSubclass: aClassRemoved classAffected)
]

{ #category : #controlling }
ClyInheritanceAnalysisEnvironmentPlugin >> processFullClassChange: aClassModificationApplied [
	
	self resetCacheOfClass: aClassModificationApplied classAffected.
	environment systemChanged: (
		ClyOverriddenSuperclassesChanged overridingSubclass: aClassModificationApplied classAffected).
	environment systemChanged: (
		ClyOverridingSubclassesChanged overriddenSuperclass: aClassModificationApplied classAffected)
]

{ #category : #controlling }
ClyInheritanceAnalysisEnvironmentPlugin >> processMethodChange: aMethodAnnouncement [
	| method |
	self resetCacheOfMethod: aMethodAnnouncement methodAffected.
	
	method := aMethodAnnouncement methodAffected.
	method origin superclass ifNotNil: [ :superclass |
		(superclass lookupSelector: method selector) ifNotNil: [:overriddenMethod |
			^environment systemChanged: (ClyOverriddenMethodChanged method: overriddenMethod)]
	].
	method origin subclasses ifEmpty: [ ^self ].
	environment systemChanged: (ClyOverridingMethodsChanged overriddenMethod: method)
]

{ #category : #'methods analysis' }
ClyInheritanceAnalysisEnvironmentPlugin >> resetCacheOfClass: aClass [
	aClass ifNil: [ ^self ].
	
	cacheGuard critical: [ | selectorsToRemove |
		selectorsToRemove := OrderedCollection new.
		methodCache keysAndValuesDo: [ :selector :classCache |
			(self doesClassCache: classCache includesOverridesOf: aClass)
					ifTrue: [ selectorsToRemove add: selector ]].
		selectorsToRemove do: [:each | methodCache removeKey: each]
	]
]

{ #category : #'methods analysis' }
ClyInheritanceAnalysisEnvironmentPlugin >> resetCacheOfMethod: aMethod [
	
	cacheGuard critical: [
		methodCache removeKey: aMethod selector ifAbsent: [  ]
	]
]
