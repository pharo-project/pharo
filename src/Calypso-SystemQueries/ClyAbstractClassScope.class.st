"
I am a root of class scope hierarchy.
From any class scope you are able access classes, methods and variables. 
And my subclasses define what exact objects are accessible.

For example ClyInstanceSideScope can only access instance side methods. While ClyClassSideScope see only class side methods.
Or ClySuperclassScope can access methods of all superclasses of basis classes. 

Class scope instances should be created on set of classes:

	ClyClassScope of: String .
	ClyClassScope ofAll: { String. Array }.

I define accessing interface with following set of methods: 

- classesDo: aBlock 
Each scope should implement visible classes enumeration

- methodsDo: aBlock
Each scope should implement visible methodes enumeration

- instanceVariablesDo: aBlock
It enumerates all instance variables available from visible classes. It is not abstract method. It is based of class enumeration.

- classVariablesDo: aBlock
It enumerates all class variables available from visible classes. It is not abstract method. It is based of class enumeration.

- variablesDo: aBlock
It enumerates all available variables from visible classes. It is not abstract.

- methodGroupsDo: aBlock 
It is special method which collects and enumerates all methods groups available for given class scope using environment plugins. The actual logic of groups colletion is explained in ClyAllMethodGroups comment.

- collectAllClasses

- includesClass: aClass

The methods enumeration is abstract because I do not apply any restriction on the visible meta level of classes.  
So for given class I do not know what methods I can access: instance side or class side, or both. 
It is responsibility of my subclasses to define concrete meta level logic and implement #methodsDo: according to it. 
So to define meta level logic subclasses should implement following methods:

- metaLevelsOf: aClass do: aBlock
It should evaluate given aBlock with all meta levels of given class which are accessible from receiver. For example ClyInstanceSideScope will evaluate aBlock with instance side of aClass. And ClyBothMetaLevelClassScope will evaluate aBlock twice with instance side and class side separately.

- localScopeClass 
It should return one of ClyLocalClassScope subclasses depending on what local scope the receiver represents.

- asLocalClassScope 
It should convert the receiver to it local scope.

- withMetaLevel: aMetaLevelScopeClass
It should convert the receiver to the similar scope but which will represent given meta level. Local scopes are converted completaly to new scope class with this method.

- adoptLocalScopeClassTo: aLocalScopeClass
It should adopt receiver to the given local scope. As opposite to the previous method it supposed to modify receiver.
It is internal method to support #asScope: convertion propertly. Idea that converted class scope should keep receiver local scope if possible. And local scope itself implement this method with empty body.

And I provide several convertion methods whichare used by tools to increase or decrease class and methods visibility:

- asInheritedScope
It return the scope based on receiver basis which provide view on all inherited classes by any definition of inheritance availably in the system. It uses class annotation ClyInheritedScopeProvider to find actual scope class which is responsible to build inheritance scope from receiver. By default it is ClySuperclassScope which includes all superclasses of basis. But with Traits plugin it will be composed scope which includes superclasses and inherited traits. 

- increasedByClassesFrom: aClassScope 
It returned similar scope to receiver but with basis increased by all classes visible from given aClassScope. 
 
- reducedByClassesFrom: aClassScope
It returned similar scope to receiver but with basis reduced by all classes visible from given aClassScope.

- asInterestingClassScope
This methods was introduced specifically to be able restrict ClySuperclassScope by excluding too common superclasses like Object and ProtoObject. For this purpose the ClyInterestingSuperclassScope was implemented. 
This method is also implemented in ClyCompositeScope. So in case when you have full class hierarchy composition scope you are also able restrict it by excluding uninteresting parts like Object.
It is used by browser to enable visibility of all inherited methods accept methods from most common classes like Object and ProtoObject.

Also I implement methods which are used in system changes processing. Queries ask me about various kind of affect which particular event could produce ob objects which are visible from me. Look at methods under tag ""system changes"".

And to support scoped refactoring my instances can be converted to the refactoring environment:
- asRBEnvironment 
"
Class {
	#name : #ClyAbstractClassScope,
	#superclass : #ClyTypedScope,
	#category : #'Calypso-SystemQueries-Scopes'
}

{ #category : #converting }
ClyAbstractClassScope class >> asInterestingClassScope [
	^self
]

{ #category : #accessing }
ClyAbstractClassScope class >> defaultName [
	^'classes'
]

{ #category : #'scope names' }
ClyAbstractClassScope class >> hierarchyScopeName [
	^'hier'
]

{ #category : #'scope names' }
ClyAbstractClassScope class >> inheritedScopeName [
	^'parents'
]

{ #category : #'scope names' }
ClyAbstractClassScope class >> inheritingScopeName [
	^'children'
]

{ #category : #'scope names' }
ClyAbstractClassScope class >> userHierarchyScopeName [
	^'user hier'
]

{ #category : #converting }
ClyAbstractClassScope >> adoptLocalScopeClassTo: aLocalScopeClass [
	self subclassResponsibility 
]

{ #category : #converting }
ClyAbstractClassScope >> asInheritedScope [
	^ClyInheritedScopeProvider createInheritedScopeFrom: self
]

{ #category : #converting }
ClyAbstractClassScope >> asInheritingScope [
	^ClyInheritedScopeProvider createInheritingScopeFrom: self
]

{ #category : #converting }
ClyAbstractClassScope >> asInterestingClassScope [
	^self
]

{ #category : #converting }
ClyAbstractClassScope >> asLocalClassScope [
	self subclassResponsibility 
]

{ #category : #'refactoring support' }
ClyAbstractClassScope >> asRBEnvironment [
	^RBClassEnvironment classes: self collectAllClasses
]

{ #category : #converting }
ClyAbstractClassScope >> asScope: aClassScopeClass [

	| newScope |
	newScope := super asScope: aClassScopeClass.
	newScope adoptLocalScopeClassTo: self localScopeClass.
	^newScope
]

{ #category : #queries }
ClyAbstractClassScope >> classVariablesDo: aBlock [

	self classesDo: [ :eachClass | 
		self classVariablesOf: eachClass do: aBlock]
]

{ #category : #private }
ClyAbstractClassScope >> classVariablesOf: definingClass do: aBlock [
	"Class variables are visible from any meta level of declaring class.
	But we can retrieve them only from instance side of given class"
	definingClass instanceSide classVariables associationsDo: [:var | 
		aBlock value: (ClyClassVariable on: var definedIn: definingClass instanceSide)]
]

{ #category : #queries }
ClyAbstractClassScope >> classesDo: aBlock [
	self subclassResponsibility 
]

{ #category : #private }
ClyAbstractClassScope >> collectAllClasses [
	| classes |
	classes := OrderedCollection new: basisObjects size.
	
	self classesDo: [ :each | classes add: each ].
	
	^classes
]

{ #category : #testing }
ClyAbstractClassScope >> includesClass: aClass [
	
	self classesDo: [ :each | each = aClass ifTrue: [^true]].
	^false
]

{ #category : #'system changes' }
ClyAbstractClassScope >> includesClassesAffectedBy: aSystemAnnouncement [

	self classesDo: [ :each | 
		(aSystemAnnouncement affectsClass: each) ifTrue: [ ^true ]].
	^false
]

{ #category : #'system changes' }
ClyAbstractClassScope >> includesMethodGroupsAffectedBy: aSystemAnnouncement [
	self
		methodGroupProvidersDo: [ :each | 
			(each providesGroupsAffectedBy: aSystemAnnouncement inScope: self)
				ifTrue: [ ^ true ] ].
	^ false
]

{ #category : #'system changes' }
ClyAbstractClassScope >> includesMethodsAffectedBy: aSystemAnnouncement [

	self classesDo: [ :each | 
		(each includesMethodsAffectedBy: aSystemAnnouncement) ifTrue: [ ^true ]].
	^false
]

{ #category : #'system changes' }
ClyAbstractClassScope >> includesVariablesAffectedBy: aSystemAnnouncement [

	self classesDo: [ :class | 
		(aSystemAnnouncement affectsVariablesOf: class) ifTrue: [^true]].
	
	^false
]

{ #category : #converting }
ClyAbstractClassScope >> increasedByClassesFrom: aClassScope [

	^self withExtraBasisObjects: aClassScope collectAllClasses
]

{ #category : #queries }
ClyAbstractClassScope >> instanceVariablesDo: aBlock [

	self classesDo: [ :eachClass | 
		self instanceVariablesOf: eachClass do: aBlock]
]

{ #category : #private }
ClyAbstractClassScope >> instanceVariablesOf: definingClass do: aBlock [

	| slots |
	self metaLevelsOf: definingClass do: [ :concreteMetaLevelClass |
		"Ugly way to support Pharo7 slots from traits"
		slots := (concreteMetaLevelClass respondsTo: #localSlots) 
			ifTrue: [ concreteMetaLevelClass localSlots]
			ifFalse: [ concreteMetaLevelClass slots ].
		slots do: [:var | 
			aBlock value: (ClyInstanceVariable on: var definedIn: definingClass) ]]
]

{ #category : #'meta level' }
ClyAbstractClassScope >> localScopeClass [
	self subclassResponsibility 
]

{ #category : #'meta level' }
ClyAbstractClassScope >> metaLevelsOf: aClass do: aBlock [
	"Subclasses should decide what class meta level is visible. 
	For example it can be instance side, class side or both"
	
	self subclassResponsibility 
]

{ #category : #private }
ClyAbstractClassScope >> methodGroupProvidersDo: aBlock [
	| providers |

	environment pluginsDo: [ :plugin | 
		providers := plugin collectMethodGroupProviders.
		providers do: aBlock ]
]

{ #category : #queries }
ClyAbstractClassScope >> methodGroupsDo: aBlock [
	| dynamicGroups groups |
	dynamicGroups := OrderedCollection new.
	
	self methodGroupProvidersDo: [ :groupProvider |
		groups := groupProvider buildGroupsFrom: self.
		groupProvider isStatic
			ifTrue: [ groups do: aBlock ]
			ifFalse: [ dynamicGroups addAll: groups ]].
		
	self processDynamicMethodGroups: dynamicGroups by: aBlock
]

{ #category : #queries }
ClyAbstractClassScope >> methodsDo: aBlock [

	self subclassResponsibility 
]

{ #category : #printing }
ClyAbstractClassScope >> printBasisObject: aClass on: aStream [

	^aStream nextPutAll: aClass name
]

{ #category : #private }
ClyAbstractClassScope >> processDynamicMethodGroups: dynamicGroups by: aBlock [

	| activeGroups |
	activeGroups := OrderedCollection new: dynamicGroups size.
	self methodsDo: [ :eachMethod |
		dynamicGroups do: [ :eachGroup | 
			(eachGroup dependsOnMethod: eachMethod) ifTrue: [ 
				aBlock value: eachGroup.
				activeGroups add: eachGroup]].
		dynamicGroups removeAll: activeGroups.
		dynamicGroups ifEmpty: [ ^self ].
		activeGroups reset].
]

{ #category : #converting }
ClyAbstractClassScope >> reducedByClassesFrom: aClassScope [

	| myClasses classesToExclude |
	myClasses := basisObjects collect: [:each | each instanceSide].
	classesToExclude := aClassScope collectAllClasses collect: [:each | each instanceSide].

	^self withNewBasisObjects: (myClasses copyWithoutAll: classesToExclude)
]

{ #category : #queries }
ClyAbstractClassScope >> variablesDo: aBlock [

	self classesDo: [ :eachClass | 
		self classVariablesOf: eachClass do: aBlock.
		self instanceVariablesOf: eachClass do: aBlock]

]

{ #category : #converting }
ClyAbstractClassScope >> withMetaLevel: aMetaLevelScopeClass [
	self subclassResponsibility 
]
