"
I am a root of scope classes which show objects accessible from the particular kind of class hierarchy. 
For example there is ClySuperclassScope with superclasses of basis classes and all their methods (inherited by basis).
And there is ClySubclassScope which shows all subclasses and their methods.

I implement all abstract methods from superclass and introduce new method which should be defined by subclasses: 

- classesRelatedTo: aClass do: aBlock

In this method subclasses should evaluate given block with all other classes which are related to given aClass according to the logic of given class hierarchy.

My varable localScopeClass specifies what part of class itself is visible. It can be instance side, class side or both with corresponsing variable values: ClyInstanceSideScope, ClyClassSideScope and ClyBothMetaLevelClassScope.
I use this variable to define methods required for scope query protocol. Look at overrides for details.

To create instance I provide several new methods where you can specify local scope class:

	ClySubclassScope of: Array localScope: ClyClassSideScope.
	ClySubclassScope of: Array in: ClyNavigationEnvironment currentImage localScope: ClyInstanceSideScope.
	ClySubclassScope ofAll: {Array. Point} localScope: ClyBothMetaLevelClassScope.
	ClySubclassScope ofAll: {Array. Point} in: ClyNavigationEnvironment currentImage  localScope: ClyClassScope.
	
Internal Representation and Key Implementation Points.

    Instance Variables
	localScopeClass:		<ClyLocalClassScope class>
"
Class {
	#name : #ClyClassHierarchyScope,
	#superclass : #ClyAbstractClassScope,
	#instVars : [
		'localScopeClass'
	],
	#category : #'Calypso-SystemQueries-Scopes'
}

{ #category : #accessing }
ClyClassHierarchyScope class >> defaultName [
	^self hierarchyScopeName 
]

{ #category : #'instance creation' }
ClyClassHierarchyScope class >> of: basisObjects in: aNavigationEnvironment localScope: aClassScopeClass [
	^ (self of: basisObjects in: aNavigationEnvironment)
		localScopeClass: aClassScopeClass
]

{ #category : #'instance creation' }
ClyClassHierarchyScope class >> of: basisObjects localScope: aClassScopeClass [
	^ (self of: basisObjects) localScopeClass: aClassScopeClass
]

{ #category : #'instance creation' }
ClyClassHierarchyScope class >> ofAll: basisObjects in: aNavigationEnvironment localScope: aClassScopeClass [
	^ (self ofAll: basisObjects in: aNavigationEnvironment)
		localScopeClass: aClassScopeClass
]

{ #category : #'instance creation' }
ClyClassHierarchyScope class >> ofAll: basisObjects localScope: aClassScopeClass [
	^ (self ofAll: basisObjects) localScopeClass: aClassScopeClass
]

{ #category : #comparing }
ClyClassHierarchyScope >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^false ].
	
	^ localScopeClass = anObject localScopeClass
]

{ #category : #converting }
ClyClassHierarchyScope >> adoptLocalScopeClassTo: aLocalScopeClass [
	localScopeClass := aLocalScopeClass
]

{ #category : #queries }
ClyClassHierarchyScope >> allClassesRelatedTo: aClass do: aBlock [
	"Any hierarchy is defined in terms of relationship between classes 
	retrieved from meta level which are specified by local scope class
	Hierarchy analyze are continue only when following condition is true:
		(localClassScope metaLevelForHierarchyOf: each) = each
	It ensures that at next iteration we will not move out of specified local class scope bounderies  
	It is important condition for complex cases when we are analyzing metaclasses.
	For example Class allSubclasses will return almost all classes in system.
	And depending on local scope we want all of them 
	or we want only actual subclasses which was defined by users.
	So only with ClassScope as local scope we will see everything.
	But concrete meta level scopes (instance side or class side) will see you only real classes
	
	Also any hierarchy scope should ignore obsolete classes"
	| actualClass |
	actualClass := localScopeClass metaLevelForHierarchyOf: aClass.
	
	self classesRelatedTo: actualClass do: [:each | 
		((localScopeClass metaLevelForHierarchyOf: each) = each) ifTrue: [ 
			each isObsolete ifFalse: [ self metaLevelsOf: each do: aBlock].
			self allClassesRelatedTo: each do: aBlock]]
]

{ #category : #converting }
ClyClassHierarchyScope >> asLocalClassScope [
	^self asScope: localScopeClass 
]

{ #category : #testing }
ClyClassHierarchyScope >> canDetectAffectOnClassesBy: aSystemAnnouncement [
	"I can not detect that removed class was in my hierarchy.
	I can not detect that removed trait from class definition was in users hierarchy of some root trait.
	For example when remove event is announced 
	then affected class is still in the list of subclasses of superclass"		
	
	^(aSystemAnnouncement isKindOf: ClassAnnouncement) not
]

{ #category : #queries }
ClyClassHierarchyScope >> classesDo: aBlock [

	basisObjects do: [ :each | 
		self allClassesRelatedTo: each do: aBlock]
]

{ #category : #queries }
ClyClassHierarchyScope >> classesRelatedTo: aClass do: aBlock [
	self subclassResponsibility 
]

{ #category : #comparing }
ClyClassHierarchyScope >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^super hash bitXor: localScopeClass hash
]

{ #category : #initialization }
ClyClassHierarchyScope >> initialize [
	super initialize.
	
	localScopeClass := ClyClassScope "By default we only look at own class meta level"
]

{ #category : #'meta level' }
ClyClassHierarchyScope >> localScopeClass [
	^ localScopeClass
]

{ #category : #'meta level' }
ClyClassHierarchyScope >> localScopeClass: aLocalScopeClass [
	localScopeClass := aLocalScopeClass
]

{ #category : #'meta level' }
ClyClassHierarchyScope >> metaLevelsOf: aClass do: aBlock [

	localScopeClass metaLevelsOf: aClass do: aBlock
]

{ #category : #queries }
ClyClassHierarchyScope >> methodsDo: aBlock [

	self classesDo: [ :eachClass | 
		eachClass methods do: aBlock ]
]

{ #category : #converting }
ClyClassHierarchyScope >> withMetaLevel: aScopeClass [ 
	^self copy 
		localScopeClass: aScopeClass
]
