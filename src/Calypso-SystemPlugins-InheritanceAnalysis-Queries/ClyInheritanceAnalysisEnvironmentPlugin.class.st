"
I plug environment with many kind of inheritance hints.

I tag abstract classes and method.
I analyze method inheritance and tag method with overriden and overriding tags.

To check if method is overriding superclass methods use following expression: 

	plugin isMethodOverriding: aMethod.

For implementation I simply call ""aMethod isOverriding"" which checks if any superclass implements same selector. It is quite fast operation due to the fact that the size of most of the hierarchies are not much than 10.

To check if method is overridden by some of subclasses methods use following expression: 

	plugin isMethodOverridden: aMethod.

The ""aMethod isOverridden"" can be very expensive. It enumerates all subclasses of the method class. And there are classes with really huge hierarchies like Object or Morph. 
To optimize overridden state analysis I build the cache of all method implementors in the image (#allImplementorsCache). It makes the analysis for big hierarchies very fast. But for small hierachies the #isOverridden operation is faster. I use the cache only when the class of the method has more than 10 subclasses in the hierarchy. Testing approves that it is a good criteria. But you can specify different value using #littleHierarchyMaxSize variable.

In addition #allImplementorsCache is built lazely only when I need to analyze big hierarchy. It avoids the effect of slow analysis in general browser experience. Users rarely open the browser on huge hierarcies. And only in such scenarious they can notice the little initial delay when browser is opening. (Testing shows just 200 ms to build the cache for vanila Pharo image on macair machine).

I am subscribed on system changes and invalidate #allImplementorsCache when related classes or methods are changed (look at #attachToSystem method for details).

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
		'cacheGuard',
		'allImplementorsCache',
		'littleHierarchyMaxSize'
	],
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries'
}

{ #category : #testing }
ClyInheritanceAnalysisEnvironmentPlugin class >> isSlow [
	^true
]

{ #category : #controlling }
ClyInheritanceAnalysisEnvironmentPlugin >> attachToSystem [
	super attachToSystem.
	
	environment system when: MethodAdded, MethodRemoved send: #processMethodChange: to: self.
	environment system when: ClassRemoved send: #processClassRemoval: to: self.
	environment system when: ClassModifiedClassDefinition send: #processClassDefinitionChange: to: self.
	environment system when: ClassModificationApplied send: #processFullClassChange: to: self.
]

{ #category : #'implementors cache' }
ClyInheritanceAnalysisEnvironmentPlugin >> cacheAllImplementors [

	cacheGuard critical: [ 
		allImplementorsCache := IdentityDictionary new: Symbol selectorTable size.
		environment systemScope methodsDo: [ :each |
			self cacheMethod: each ]
	]


]

{ #category : #'implementors cache' }
ClyInheritanceAnalysisEnvironmentPlugin >> cacheMethod: aMethod [
	| classes |
	classes := self implementorsOf: aMethod.
	classes add: aMethod methodClass 

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

{ #category : #'implementors cache' }
ClyInheritanceAnalysisEnvironmentPlugin >> implementorsOf: aMethod [
	allImplementorsCache ifNil: [ self cacheAllImplementors ].
	
	^allImplementorsCache at: aMethod selector ifAbsentPut: [ WeakOrderedCollection new ]
	"Classes implementing each selector are cached using weak structure.
	It allows to avoid cleaning cache when user removes the class.
	As consequence nobody needs anymore to test the presence of given class in these caches.
	it allows to use WeakOrderedCollection here 
	which is much faster for building full implementors cache.
	Notice that WeakOrderedCollection requires to be carefull with enumerating items 
	because they can become nil by garbage collector"
]

{ #category : #initialization }
ClyInheritanceAnalysisEnvironmentPlugin >> initialize [
	super initialize.
	
	cacheGuard := Mutex new.
	littleHierarchyMaxSize := 10
]

{ #category : #'methods analysis' }
ClyInheritanceAnalysisEnvironmentPlugin >> isClassCheapForOverriddenMethodsAnalysis: aClass [
	"Building cache of all implementors is expensive operation 
	and we are trying to avoid it until it becomes really necessary for performance.
	Here we are using a simple criteria to detect that we can avoid the cache.
	When there are small amount of subclasses if it always cheap operation without any cache"
		
	| numberOfSubclasses |
	numberOfSubclasses := 0.
	aClass allSubclassesDo: [ :each | 
		numberOfSubclasses > littleHierarchyMaxSize ifTrue: [ ^false ].
		numberOfSubclasses := numberOfSubclasses + 1].
	^true
]

{ #category : #'methods analysis' }
ClyInheritanceAnalysisEnvironmentPlugin >> isMethodOverridden: aMethod [
	| classes methodClass |
	"Implementors cache optimizes analyzis only for classes with a lot of subclasses.
	For example Object and Morph are really profit from it.
	But for simple hierarchies the direct overridden state analysis is quite cheap operation.
	And it's much faster than processing using cache.
	In addition using cheap processing path allows to defer the cache building.
	It removes the effect of initial slow analysis from general browser experience 
	where user rarely browse the root classes of big hierarchies"
	(self isClassCheapForOverriddenMethodsAnalysis: aMethod methodClass) ifTrue: [ 
			^aMethod isOverridden ].
	
	classes := self implementorsOf: aMethod.	
	methodClass := aMethod methodClass.
	^classes anySatisfy: [ :each | 
		each notNil and: [(each inheritsFrom: methodClass) and: [ each isObsolete not ]]]
]

{ #category : #'methods analysis' }
ClyInheritanceAnalysisEnvironmentPlugin >> isMethodOverriding: aMethod [
	"We do not use implementors cache for overriding state analysis 
	because the direct method #isOverriding is much faster"
	^aMethod isOverriding
]

{ #category : #accessing }
ClyInheritanceAnalysisEnvironmentPlugin >> littleHierarchyMaxSize [
	^ littleHierarchyMaxSize
]

{ #category : #accessing }
ClyInheritanceAnalysisEnvironmentPlugin >> littleHierarchyMaxSize: anObject [
	littleHierarchyMaxSize := anObject
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
	
	environment systemChanged: (
		ClyOverriddenSuperclassesChanged overridingSubclass: aClassDefinitionChange oldClassDefinition)
]

{ #category : #controlling }
ClyInheritanceAnalysisEnvironmentPlugin >> processClassRemoval: aClassRemoved [
	"We do not update cache when class is removed 	because it will slow down removal operation.
	(it requires enumeration of all key elements (~54000 selectors in the image)
	The cleaning is based on weak registry for classes 
	and cache analysis skips obsolete classes when they are still in cache"
	
	environment systemChanged: (
		ClyOverriddenSuperclassesChanged overridingSubclass: aClassRemoved classAffected)
]

{ #category : #controlling }
ClyInheritanceAnalysisEnvironmentPlugin >> processFullClassChange: aClassModificationApplied [
	
	environment systemChanged: (
		ClyOverriddenSuperclassesChanged overridingSubclass: aClassModificationApplied classAffected).
	environment systemChanged: (
		ClyOverridingSubclassesChanged overriddenSuperclass: aClassModificationApplied classAffected)
]

{ #category : #controlling }
ClyInheritanceAnalysisEnvironmentPlugin >> processMethodChange: aMethodAnnouncement [
	| method |
	self updateCacheForMethod: aMethodAnnouncement methodAffected.
	
	method := aMethodAnnouncement methodAffected.
	method methodClass superclass ifNotNil: [ :superclass |
		(superclass lookupSelector: method selector) ifNotNil: [:overriddenMethod |
			^environment systemChanged: (ClyOverriddenMethodChanged method: overriddenMethod)]
	].
	method methodClass subclasses ifEmpty: [ ^self ].
	environment systemChanged: (
		ClyOverridingMethodsChanged initiatedBy: self forOverriddenMethod: method)
]

{ #category : #'implementors cache' }
ClyInheritanceAnalysisEnvironmentPlugin >> removeMethodFromCache: aMethod [
	
	| classes |
	classes := allImplementorsCache at: aMethod selector ifAbsent: [^self].
	classes remove: aMethod methodClass ifAbsent: [ ].
	"Classes are WeakOrderedCollection 
	and when it is cleaned by GS it does not become empty. All items are just replaced by nil"
	(classes isEmpty or: [ 
		classes allSatisfy: [:each | each isNil or: [ each isObsolete ] ]])
			ifTrue: [allImplementorsCache removeKey: aMethod selector]
]

{ #category : #'implementors cache' }
ClyInheritanceAnalysisEnvironmentPlugin >> resetImplementorsCache [

	cacheGuard critical: [ 
		allImplementorsCache := nil.
	]


]

{ #category : #'implementors cache' }
ClyInheritanceAnalysisEnvironmentPlugin >> updateCacheForMethod: aMethod [
	allImplementorsCache ifNil: [ ^self ].
	
	cacheGuard critical: [
		aMethod isInstalled 
			ifTrue: [ self cacheMethod: aMethod ]
			ifFalse: [ self removeMethodFromCache: aMethod]				
	]
]
