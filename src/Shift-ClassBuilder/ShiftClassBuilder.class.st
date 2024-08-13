"
I know how to create classes in the system.
The classes are nor installed or modifies other objects. That is part of the job of the ShiftClassInstaller.

I can be extended by using a different builder enhancer. 
See ShDefaultBuilderEnhancer for a default implementation. 

I can be used directly to create anonymous classes, but it is better if you use the anonymous class installer.

I also can compare the old class with the configured new class to calculate the required changes.

I'm responsible to build class expressed with the fluid syntax.

```st
Object << #Point 
	slots: { #x . #y };
	tag: 'Basics';
	package: 'Kernel'
```	
	
### Design decisions

The KEY design decisions are:
- The code snippet above should be able to be used for scripting (interactively).
- The code snippet above must return a class builder 
- A class builder should offer two important messsage: 
-- `build` that returns a class but does not install it. 
-- `install` that installs the class in the system.

To manage class side slots and traits, I get all the instance side information in addition to the class side one.
For this the fillFromClass: method is responsible to convert all the class information into an information expected by the class builder. 

In the future we would like to revisit the need for such fluid class builders and also revisit the API of the class builder.There are too many conversion and we should realigned the APIs with the class default values too. 
"
Class {
	#name : 'ShiftClassBuilder',
	#superclass : 'Object',
	#instVars : [
		'buildEnvironment',
		'installingEnvironment',
		'superclassName',
		'name',
		'layoutDefinition',
		'comment',
		'commentStamp',
		'superclass',
		'newMetaclass',
		'newClass',
		'oldClass',
		'oldMetaclass',
		'builderEnhancer',
		'extensibleProperties',
		'changeComparers',
		'changes',
		'metaSuperclass',
		'superclassResolver',
		'inRemake',
		'package',
		'tag'
	],
	#classVars : [
		'BuilderEnhancer'
	],
	#category : 'Shift-ClassBuilder-Builder',
	#package : 'Shift-ClassBuilder',
	#tag : 'Builder'
}

{ #category : 'defaults' }
ShiftClassBuilder class >> defaultBuildEnhancer [
	^ BuilderEnhancer ifNil: [ ShDefaultBuilderEnhancer ]
]

{ #category : 'accessing' }
ShiftClassBuilder class >> setDefaultBuilderEnhancer: aClass [
	BuilderEnhancer := aClass
]

{ #category : 'changes' }
ShiftClassBuilder >> addChange: aChange [
	changes add: aChange
]

{ #category : 'changes' }
ShiftClassBuilder >> addChangeComparer: aChangeComparer [
	changeComparers add: aChangeComparer
]

{ #category : 'accessing' }
ShiftClassBuilder >> allSlots [

	^ self builderEnhancer allSlotsForBuilder: self
]

{ #category : 'building' }
ShiftClassBuilder >> build [

	self tryToFillOldClass.
	self detectBuilderEnhancer.
	self builderEnhancer validateRedefinition: self oldClass.

	self validateSuperclass.
	self compareWithOldClass.

	"If this is the first build, when building a class.
	We need to check if there is no conflicts with existing subclasses.
	If we are in a remake, it have been done when building the superclass modified before."

	self isInRemake ifFalse: [ self layoutDefinition validate ].

	self createMetaclass.
	self createClass.

	self createSharedVariables.

	self installSlotsAndVariables.

	self oldClass ifNotNil: [ self builderEnhancer compileMethodsFor: self ].

	self builderEnhancer afterMethodsCompiled: self.
	^ newClass
]

{ #category : 'accessing' }
ShiftClassBuilder >> buildEnvironment [
	^ buildEnvironment
]

{ #category : 'accessing' }
ShiftClassBuilder >> buildEnvironment: anObject [
	buildEnvironment := anObject
]

{ #category : 'accessing' }
ShiftClassBuilder >> builderEnhancer [
	^ builderEnhancer ifNil: [ self detectBuilderEnhancer ]
]

{ #category : 'accessing' }
ShiftClassBuilder >> builderEnhancer: anObject [
	builderEnhancer := anObject.

	builderEnhancer initializeBuilder: self
]

{ #category : 'accessing' }
ShiftClassBuilder >> changeComparers [
	^ changeComparers
]

{ #category : 'accessing' }
ShiftClassBuilder >> changes [
	^ changes
]

{ #category : 'accessing' }
ShiftClassBuilder >> classNamed: aName [

	^ self buildEnvironment at: aName ifAbsent: [ nil ]
]

{ #category : 'accessing' }
ShiftClassBuilder >> classSlots [
	^ self layoutDefinition classSlots
]

{ #category : 'accessing' }
ShiftClassBuilder >> classSlots: aSlotCollection [
	self layoutDefinition classSlots: aSlotCollection
]

{ #category : 'accessing' }
ShiftClassBuilder >> comment [
	^ comment
]

{ #category : 'accessing' }
ShiftClassBuilder >> comment: anObject [
	comment := anObject
]

{ #category : 'accessing - comment' }
ShiftClassBuilder >> comment:aComment stamp: anStamp [
	self comment: aComment.
	self commentStamp: anStamp
]

{ #category : 'accessing' }
ShiftClassBuilder >> commentStamp [
	^ commentStamp
]

{ #category : 'accessing' }
ShiftClassBuilder >> commentStamp: anObject [
	commentStamp := anObject
]

{ #category : 'changes' }
ShiftClassBuilder >> compareWithOldClass [
	"Comparing changes only needs to be done when updating (rebuilding) and existing class"
	self isRebuild ifFalse: [ ^ self ].
	
	changeComparers do: [ :e | e compareClass: oldClass with: self ].
	changes isEmpty ifTrue: [ ShNoChangesInClass signal ]
]

{ #category : 'compiling' }
ShiftClassBuilder >> compileMethods [

	newClass
		compileAllFrom: self oldClass;
		removeNonexistentSelectorsFromProtocols
]

{ #category : 'building' }
ShiftClassBuilder >> copyProtocols [

	newClass protocols: oldClass protocols copy.
	newClass class protocols: oldClass class protocols copy
]

{ #category : 'installing' }
ShiftClassBuilder >> createClass [

	newClass := newMetaclass new.
	newClass setName: self name.

	self builderEnhancer
		configureClass: newClass
		superclass: self superclass
		withLayoutType: self layoutClass
		slots: (self withAdditionalSlots: self slots).

	newClass environment: self installingEnvironment.
	
	"Since Traits might add some methods in #classCreated:, we need to copy the protocols before we add them."
	self oldClass ifNotNil: [
		self copyProtocols.
		self newClass commentSourcePointer: self oldClass commentSourcePointer ].

	self builderEnhancer classCreated: self
]

{ #category : 'building' }
ShiftClassBuilder >> createMetaclass [

	newMetaclass := self metaclassClass new.

	self builderEnhancer
		configureMetaclass: newMetaclass
		superclass: self metaSuperclass
		withLayoutType: FixedLayout
		slots: (self withAdditionalSlots: self classSlots)
]

{ #category : 'building' }
ShiftClassBuilder >> createSharedVariables [

	self builderEnhancer on: newClass declareClassVariables: self layoutDefinition sharedVariables sharing: self layoutDefinition sharedPools
]

{ #category : 'initialization' }
ShiftClassBuilder >> detectBuilderEnhancer [

	| builderEnhancerClass |

	builderEnhancerClass := ShDefaultBuilderEnhancer allSubclasses detect: [ :e | e isApplicableFor: self ] ifNone: [ self class defaultBuildEnhancer ].
	builderEnhancer := builderEnhancerClass new.

	builderEnhancer initializeBuilder: self.
	^ builderEnhancer
]

{ #category : 'reflective operations' }
ShiftClassBuilder >> doesNotUnderstand: aMessage [
	| selector variable setter|

	selector := aMessage selector.

	(selector isUnary or:[ selector isKeyword and:[ selector keywords size = 1] ])
		ifFalse:[ ^ super doesNotUnderstand: aMessage].

	setter := selector isKeyword.
	variable := setter ifTrue:[(selector allButLast: 1) asSymbol] ifFalse:[selector].

	(extensibleProperties includesKey: variable)
		ifFalse: [ ^ super doesNotUnderstand: aMessage ].

	setter
		ifTrue:[ extensibleProperties at: variable put: aMessage argument]
		ifFalse:[ ^ extensibleProperties at: variable]
]

{ #category : 'accessing' }
ShiftClassBuilder >> environment [

	^ self buildEnvironment
]

{ #category : 'accessing' }
ShiftClassBuilder >> environment: anObject [

	^ self buildEnvironment: anObject
]

{ #category : 'copying' }
ShiftClassBuilder >> fillClassSideFromEnvironment: anEnvironment [

	| old |
	old := anEnvironment at: name ifAbsent: [ ^ self ].

	self classSlots: old class slots
]

{ #category : 'initialization' }
ShiftClassBuilder >> fillFor: aClass [

	self
		superclass: aClass superclass;
		name: aClass getName;
		layoutClass: aClass classLayout class;
		slots: (aClass localSlots collect: [:var | var copy]);
		classSlots: (aClass class localSlots collect: [:var | var copy]);
		sharedVariables: (aClass classVariables collect: [:var | var copy]);
		sharedPools: aClass sharedPools asArray;
		package: aClass package name;
		installingEnvironment: aClass environment;
		oldClass: aClass.

	aClass packageTag isRoot ifFalse: [ self tag: aClass packageTag name ].

	self builderEnhancer fillBuilder: self from: aClass
]

{ #category : 'copying' }
ShiftClassBuilder >> fillInstanceSideFromClass: aClass [

	<reflection: 'Class structural modification - Fluid Builder class creation'>
	self privateSlots: aClass slots.
	self privateTraitComposition: aClass traitComposition.
	self package: aClass package name.

	aClass packageTag ifNotNil: [ :aTag | aTag isRoot ifFalse: [ self tag: aTag name ] ].

	self superclass: aClass superclass.

	self layout: aClass classLayout class.

	self sharedVariables: aClass classVariables.
	self sharedPools: aClass sharedPools asArray
]

{ #category : 'building' }
ShiftClassBuilder >> fluidInstall [
	"Install the class in the system environment, method #fluidInstall is implemented in behavior, too"

	^ self install
]

{ #category : 'testing' }
ShiftClassBuilder >> hasToMigrateInstances [
	^ self changes anySatisfy: [ :e | e hasToMigrateInstances ]
]

{ #category : 'initialization' }
ShiftClassBuilder >> initialize [

	super initialize.

	self useStrictSuperclass.
	superclassName := #Object.

	layoutDefinition := ShLayoutDefinition new.
	layoutDefinition builder: self.

	extensibleProperties := Dictionary new.

	changeComparers := OrderedCollection new.
	changes := Set new.

	inRemake := false.

	buildEnvironment := self class environment
]

{ #category : 'actions' }
ShiftClassBuilder >> install [
	"Install the class in the system environment"

	^ ShiftClassInstaller new makeWithBuilder: self
]

{ #category : 'building' }
ShiftClassBuilder >> installSlotsAndVariables [
	"notify all the variables so they can react to being installed in a class"
	newClass classLayout slots do: [ :each | each installingIn: newClass ].
	newClass class classLayout slots do: [ :each | each installingIn: newClass class ].
	newClass classVariables do: [ :each | each installingIn: newClass ]
]

{ #category : 'accessing' }
ShiftClassBuilder >> installingEnvironment [
	"The build environment is used to find the classes used during the building of a class such as the layouts, and the installing environment is the environment in which the class should be installed."

	^ installingEnvironment ifNil: [ self buildEnvironment ]
]

{ #category : 'accessing' }
ShiftClassBuilder >> installingEnvironment: anObject [

	installingEnvironment := anObject
]

{ #category : 'accessing' }
ShiftClassBuilder >> isInRemake [

	"If the builder is in remake (when propagating changes to subclasses)"

	^ inRemake
]

{ #category : 'testing' }
ShiftClassBuilder >> isRebuild [
	"Are we rebuilding an existing class?"
	oldClass ifNil: [ ^false ].
	oldClass isAnonymous ifTrue: [ ^true ].
	^name = oldClass name
]

{ #category : 'accessing' }
ShiftClassBuilder >> layout: aLayoutClass [
	"Added while merging FluidClassBuilder API and ShiftClassBuilder. Maybe we should deprecate #layoutClass:"

	^ self layoutClass: aLayoutClass
]

{ #category : 'accessing' }
ShiftClassBuilder >> layoutClass [

	^ self layoutDefinition layoutClass
]

{ #category : 'accessing' }
ShiftClassBuilder >> layoutClass: aLayoutClass [
	self layoutDefinition layoutClass: aLayoutClass
]

{ #category : 'accessing' }
ShiftClassBuilder >> layoutDefinition [
	^ layoutDefinition
]

{ #category : 'accessing' }
ShiftClassBuilder >> markIsInRemake [
	inRemake := true
]

{ #category : 'accessing' }
ShiftClassBuilder >> metaSuperclass [

	^ metaSuperclass ifNil: [
		  self superclass
			  ifNil: [ Class ]
			  ifNotNil: [ self superclass class ] ]
]

{ #category : 'accessing' }
ShiftClassBuilder >> metaSuperclass: aClass [

	metaSuperclass := aClass
]

{ #category : 'accessing' }
ShiftClassBuilder >> metaclassClass [
	"The metaclass class is determined by the builder enhancer. In case you want to play with your own metaclass class, you can implement a subclass of the buildre enhancer and use this one overriding the method #metaclassClassFor:."

	^ self builderEnhancer metaclassClassFor: self
]

{ #category : 'accessing' }
ShiftClassBuilder >> name [
	^ name
]

{ #category : 'accessing' }
ShiftClassBuilder >> name: anObject [
	name := anObject.
	self validateClassName
]

{ #category : 'accessing' }
ShiftClassBuilder >> newClass [
	^ newClass
]

{ #category : 'accessing' }
ShiftClassBuilder >> newMetaclass [
	^ newMetaclass
]

{ #category : 'changes' }
ShiftClassBuilder >> notifyChanges [
	changes do: #announceChanges
]

{ #category : 'accessing' }
ShiftClassBuilder >> oldClass [
	^ oldClass
]

{ #category : 'accessing' }
ShiftClassBuilder >> oldClass: anObject [
	oldClass := anObject.
	oldClass ifNotNil: [oldMetaclass := oldClass class]
]

{ #category : 'accessing' }
ShiftClassBuilder >> oldMetaclass [
	^ oldMetaclass
]

{ #category : 'accessing' }
ShiftClassBuilder >> package [

	^ package
]

{ #category : 'accessing' }
ShiftClassBuilder >> package: aString [

	package := aString
]

{ #category : 'private' }
ShiftClassBuilder >> privateSlots: aCollection [
	self layoutDefinition slots: aCollection
]

{ #category : 'changes' }
ShiftClassBuilder >> propagateChangesTo: anotherBuilder [
	changes do: [ :e | e propagateToSubclasses: anotherBuilder ]
]

{ #category : 'accessing' }
ShiftClassBuilder >> propertyAt: aKey [
	^ extensibleProperties at: aKey
]

{ #category : 'accessing' }
ShiftClassBuilder >> propertyAt: aKey put: aValue [
	extensibleProperties at: aKey put: aValue
]

{ #category : 'accessing' }
ShiftClassBuilder >> sharedPools [
	^ self layoutDefinition sharedPools
]

{ #category : 'accessing' }
ShiftClassBuilder >> sharedPools: aCollectionOrString [
	"The paramter can be either a collection of symbols or a string that should be converted into a collection of symbols, or a Class where we get the name"
	| pools |
	pools := aCollectionOrString isString
			 ifTrue: [ (aCollectionOrString substrings: ' ') collect: [ :e | e asSymbol ] ]
			 ifFalse: [ aCollectionOrString collect: [ :each | each isSymbol ifFalse: [ each name ] ifTrue: [ each ] ].	 ].
	
	self layoutDefinition sharedPools: pools.
]

{ #category : 'accessing' }
ShiftClassBuilder >> sharedPoolsFromString: aCollectionOrString [
	"This should be only used for old style class defs"
	| pools |
	pools := (aCollectionOrString substrings: ' ') collect: [ :e | e asSymbol ].
	
	self layoutDefinition sharedPools: pools.
]

{ #category : 'accessing' }
ShiftClassBuilder >> sharedVariables [
	^ self layoutDefinition sharedVariables
]

{ #category : 'accessing' }
ShiftClassBuilder >> sharedVariables: aCollection [
	self layoutDefinition sharedVariables: (aCollection collect:[:e | e asClassVariable])
]

{ #category : 'accessing' }
ShiftClassBuilder >> sharedVariablesFromString: aString [
	"Note: this method should not be used outside of old style class definitions"
	layoutDefinition sharedVariables: aString asClassVariableCollection
]

{ #category : 'accessing' }
ShiftClassBuilder >> slots [
	^ self layoutDefinition slots
]

{ #category : 'accessing' }
ShiftClassBuilder >> slots: aCollection [

	self privateSlots: aCollection
]

{ #category : 'accessing' }
ShiftClassBuilder >> slotsFromString: aString [
	"Note: this method should not be used outside of old style class definitions"
	self slots: aString asSlotCollection
]

{ #category : 'accessing' }
ShiftClassBuilder >> superclass [

	^ superclass ifNil: [ superclassName ifNotNil: [ self classNamed: self superclassName ] ]
]

{ #category : 'accessing' }
ShiftClassBuilder >> superclass: aSuperclass [

	superclass := aSuperclass.
	superclassName := superclass ifNotNil: [ superclass name ]
]

{ #category : 'accessing' }
ShiftClassBuilder >> superclassName [
	^ superclassName
]

{ #category : 'accessing' }
ShiftClassBuilder >> superclassName: anObject [

	superclassName := anObject ifNotNil: [ anObject asSymbol ]
]

{ #category : 'accessing' }
ShiftClassBuilder >> superclassResolver: asuperclassResolver [
	superclassResolver:= asuperclassResolver
]

{ #category : 'accessing' }
ShiftClassBuilder >> tag [

	^ tag
]

{ #category : 'accessing' }
ShiftClassBuilder >> tag: anObject [

	tag := anObject
]

{ #category : 'building' }
ShiftClassBuilder >> tryToFillOldClass [

	oldClass ifNotNil: [ ^ self ].
	name ifNil: [ ^ self ].
	self oldClass: (self classNamed: self name)
]

{ #category : 'building' }
ShiftClassBuilder >> useStrictSuperclass [
	"default behavior"
	"If I am use, the superResolver will resolve unknown superclass by raising an error"
	self superclassResolver: ClassResolverStrictResolve new
]

{ #category : 'private' }
ShiftClassBuilder >> validateClassName [
	name ifNil: [ ^self ].

	"I try to convert to symbol, if there is an error the next guard will catch it"
	[ name := name asSymbol ] on: Error do: [  ].

	name isSymbol ifFalse:[InvalidGlobalName
				signal: 'Global names should be symbols'
				for: name].

	name isValidGlobalName ifFalse: [
		InvalidGlobalName signal: 'Class name is not a valid global name. It must start with uppercase letter and continue with alphanumeric characters or underscore. Default names used in class or trait templates are not allowed.' for: name ].

	DangerousClassNotifier check: name
]

{ #category : 'private' }
ShiftClassBuilder >> validateSuperclass [
	self superclass ifNil: [ ^self ].
	oldClass ifNil: [ ^ self ].

	self superclass withAllSuperclassesDo: [ :aSuperclass |
		aSuperclass = oldClass ifTrue:[
			CircularHierarchyError signalFor: oldClass ]]
]

{ #category : 'private' }
ShiftClassBuilder >> withAdditionalSlots: aSlotCollection [
	"Some slots are relying on additional slots for internal behavior. This method adds them to the list of slots of the class."

	| slots |
	slots := aSlotCollection asOrderedCollection.

	aSlotCollection do: [ :slot | slot addAdditionalSlotsTo: slots ].

	^ slots asArray
]
