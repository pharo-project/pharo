"
I add a number of facilities to those in ClassDescription:
	A set of all my subclasses (defined in ClassDescription, but only used here and below)
	A name by which I can be found in a SystemDictionary
	A classPool for class variables shared between this class and its metaclass
	A list of sharedPools which probably should be supplanted by some better mechanism.

My instances describe the representation and behavior of objects. I add more comprehensive programming support facilities to the basic attributes of Behavior and the descriptive facilities of ClassDescription.

The slot 'subclasses' is a redundant structure.  It is never used during execution, but is used by the development system to simplify or speed certain operations.

When a class is removed from the system, we keep the instance in case it is still referenced. In that case, we declare the class as Obsolete.
In order to know if a class is obsolete or not, we save the instances in a weak identity set called ObsoleteClasses. When the class is not referenced anymore, the GC will collect the class and remove it totally from the system.
Note: In the past we saved this information in the properties of the class but this is slower than the current solution. Maybe we could speed up the properties management.
"
Class {
	#name : 'Class',
	#superclass : 'ClassDescription',
	#instVars : [
		'subclasses',
		'name',
		'classPool',
		'sharedPools',
		'environment',
		'category',
		'commentSourcePointer'
	],
	#classVars : [
		'ObsoleteClasses'
	],
	#category : 'Kernel-Classes',
	#package : 'Kernel',
	#tag : 'Classes'
}

{ #category : 'file in/out' }
Class class >> allSuperclassesFor: aClass cache: cache [
	^ cache at: aClass ifAbsentPut: [aClass allSuperclasses asArray]
]

{ #category : 'file in/out' }
Class class >> doesNotIncludeInstanceOrSuperclassesFor: aClass in: unprocessedClasses cache: cache [
	| soleInstance |
	soleInstance := aClass soleInstance.
	^ (unprocessedClasses includes: soleInstance) not and: [
				self hasNoSuperclassesOf: soleInstance in: unprocessedClasses cache: cache]
]

{ #category : 'private' }
Class class >> hasNoDependenciesFor: aClass in: unprocessedClasses cache: cache [
	^ (self hasNoSuperclassesOf: aClass in: unprocessedClasses cache: cache) and: [
		aClass isMeta not or: [
			self hasNoDependenciesForMetaclass: aClass in: unprocessedClasses cache: cache]]
]

{ #category : 'file in/out' }
Class class >> hasNoDependenciesForMetaclass: aClass in: unprocessedClasses cache: cache [
	| soleInstance |
	soleInstance := aClass soleInstance.
	^ (unprocessedClasses includes: soleInstance) not and: [
				self hasNoSuperclassesOf: soleInstance in: unprocessedClasses cache: cache]
]

{ #category : 'file in/out' }
Class class >> hasNoSuperclassesOf: aClass in: unprocessedClasses cache: cache [
	^ (unprocessedClasses includesAnyOf: (self allSuperclassesFor: aClass cache: cache)) not
]

{ #category : 'class initialization' }
Class class >> initialize [

	ObsoleteClasses := WeakIdentitySet new
]

{ #category : 'file in/out' }
Class class >> superclassOrder: classes [
    "Arrange the classes in the collection, classes, in superclass order so the
    classes can be properly filed in. Do it in sets instead of ordered collections.
    Preserve provided classes order when there is no hierarchy relationship."

    | all list unprocessedClasses cache |
    list := classes copy asArray. "list is indexable"
    unprocessedClasses := classes asSet.
    cache := Dictionary new.
    all := OrderedCollection new: unprocessedClasses size.
    unprocessedClasses size timesRepeat:
        [ | nextClass nextClassIndex |
            nextClassIndex := list findFirst: [:aClass | aClass notNil
                and: [(unprocessedClasses includes: aClass)
                and: [self hasNoDependenciesFor: aClass in: unprocessedClasses cache: cache]]].
            nextClass := list at: nextClassIndex.
            list at: nextClassIndex put: nil.
            all addLast: nextClass.
            unprocessedClasses remove: nextClass].
    ^all
]

{ #category : 'accessing' }
Class class >> unclassifiedCategory [

	^ #Unclassified
]

{ #category : 'class variables' }
Class >> addClassVarNamed: aString [
	"Add the argument, aString, as a class variable of the receiver.
	Signal an error if the first character of aString is not capitalized,
	or if it is already a variable named in the class."
	<reflection: 'Class structural modification - Class variable modification'>
	self addClassVariable: (aString asSymbol => ClassVariable)
]

{ #category : 'class variables' }
Class >> addClassVariable: aClassVariable [
	"Add the argument, aString, as a class variable of the receiver.
	Signal an error if the first character of aString is not capitalized,
	or if it is already a variable named in the class."
	<reflection: 'Class structural modification - Class variable modification'>
	| symbol oldState |
	oldState := self copyForAnnouncement.
	symbol := aClassVariable name asSymbol.
	self withAllSubclasses do: [:subclass |
		(subclass bindingOf: symbol) ifNotNil:[
			^ self error: symbol asString
				, ' is already used as a variable name in class '
				, subclass name]].
	self basicDeclareClassVariable: aClassVariable.
	SystemAnnouncer uniqueInstance
			classDefinitionChangedFrom: oldState to: self;
			classModificationAppliedTo: self
]

{ #category : 'instance variables' }
Class >> addInstVarNamed: aString [
	"Add the argument, aString, as one of the receiver's instance variables."
	<reflection: 'Class structural modification - Instance variable modification'>
	aString substrings do: [ :each|
		self addSlot: (InstanceVariableSlot named: each asSymbol) ]
]

{ #category : 'pool variables' }
Class >> addSharedPool: aSharedPool [

	"Add the argument, aSharedPool, as one of the receiver's shared pools.
	Create an error if the shared pool is already one of the pools.
	This method will work with shared pools that are plain Dictionaries or thenewer SharedPool subclasses"
	<reflection: 'Class structural modification - Shared pool modification'>

	(self sharedPools includes: aSharedPool) ifTrue: [
		^ self error: 'This is already in my shared pool list' ].
	sharedPools
		ifNil: [ sharedPools := (OrderedCollection with: aSharedPool) ]
		ifNotNil: [ self sharedPools add: aSharedPool ]
]

{ #category : 'pool variables' }
Class >> addSharedPoolNamed: aSharedPoolName [
	<reflection: 'Class structural modification - Shared pool modification'>
	|poolClass|
	poolClass := aSharedPoolName asClassIfAbsent: [nil].
	poolClass isPool
		ifTrue: [ self addSharedPool: poolClass ]
		ifFalse: [ self error: 'The specified class is not pool.' ]
]

{ #category : 'accessing - class hierarchy' }
Class >> addSubclass: aSubclass [
	"Make the argument, aSubclass, be one of the subclasses of the receiver.
	Create an error notification if the argument's superclass is not the receiver."
	<reflection: 'Class structural modification - Hierarchy modification'>
	aSubclass superclass ~~ self
		ifTrue: [^self error: aSubclass name , ' is not my subclass'].
	subclasses ifNil: [
		self subclasses: (Array with: aSubclass).
		^ self ].
	self subclasses do:[:cl| cl == aSubclass ifTrue:[^self]]. "Already my subclass"
	self subclasses: (subclasses copyWith: aSubclass)
]

{ #category : 'class variables' }
Class >> allClassVariables [
	"Answer the meta objects of all class variables of the class and its superclass"

	^self withAllSuperclasses flatCollect:  [ :each | each classVariables ]
]

{ #category : 'pool variables' }
Class >> allSharedPools [
	"Answer an ordered collection of the pools the receiver shares, including those defined  in the superclasses of the receiver."
	<reflection: 'Class structural inspection - Shared pool inspection'>
	^self superclass
		ifNil: [self sharedPools copy]
		ifNotNil: [ | aSet |
			aSet := self superclass allSharedPools.
			aSet addAll: self sharedPools; yourself]
]

{ #category : 'class variables' }
Class >> anyUserOfClassVarNamed: aSymbol [
	self withAllSubclasses do: [ :subclass |
 		{subclass . subclass class} do: [ :classOrMeta |
			(classOrMeta whichMethodsReferTo: (self classPool associationAt: aSymbol))
				ifNotEmpty: [ ^classOrMeta ]]].
	^nil
]

{ #category : 'deprecation' }
Class >> applyDeprecation [

	self deprecationRefactorings do: #execute
]

{ #category : 'accessing' }
Class >> basicCategory: aSymbol [
	category := aSymbol
]

{ #category : 'class variables' }
Class >> basicDeclareClassVariable: aClassVariable [
	"Add aClassVariable to the receiver. Two cases are handled here:
	- When there is exiting variable in class pool and it is an instance of different class
	- When given variable is registered as undeclared
	This method was extracted from existing users and it keeps known issues (therefore a flag):
	- When a class of a variable is changed we must recompile using methods because new variable can implement specific compilation logic (method bytecode can become different).
	- Undeclared case requires much more clever logic. Now in that case the class variable will be shared between all classes previously referencing undeclared variable despite on the fact that they do no see a class variables from other classes"
	self flag: #todo.

	classPool ifNil: [ classPool := Dictionary new ].
	aClassVariable owningClass: self.

	self classPool associationAt: aClassVariable name ifPresent:  [:existingVar |
		(existingVar class == aClassVariable class) ifTrue: [^self].
		"need to take care to migrate existing variables to new global if class if different"
		aClassVariable write: existingVar read.
		self classPool removeKey: existingVar name ].
	"Pick up any refs in Undeclared"
	Undeclared associationAt: aClassVariable name ifPresent:  [:existingVar |
		Undeclared removeKey: existingVar name.
		existingVar becomeForward: aClassVariable
	].
	self classPool add: aClassVariable
]

{ #category : 'compiling' }
Class >> binding [
   "Answer a binding for the receiver, sharing if possible"
   | binding |
	binding := self environment associationAt: self name ifAbsent: [LiteralVariable key: nil value: self].
   ^binding value == self ifTrue: [binding] ifFalse: [LiteralVariable key: nil value: self]
]

{ #category : 'compiling' }
Class >> bindingOf: varName [
	"Answer the binding of some variable resolved in the scope of the receiver, or nil
	if variable with such name is not defined"

	"The lookup recurses up to superclasses looking inside their class and shared pools,
	but not the environment, since two classes, even if they have ancestry relationship,
	could use different environments.
	That's why we doing an environment lookup only as a last step of symbol lookup
	and taking only the environment of receiver only, not any of it's superclass(es) "
	<reflection: 'Class structural inspection - Variable lookup'>
	| aSymbol |
	aSymbol := varName asSymbol.

	^ (self innerBindingOf: aSymbol) ifNil: [
		 self environment lookupVar: aSymbol
	]
]

{ #category : 'organization' }
Class >> category [
	"Answer the system organization category for the receiver."

	^ category
]

{ #category : 'organization' }
Class >> category: aString [
	"Categorize the receiver under the system category, aString, removing it from
	any previous categorization."

	| oldCategory |
	oldCategory := self category.
	oldCategory = aString ifTrue: [ ^ self ].
	self basicCategory: aString asSymbol.

	oldCategory = Class unclassifiedCategory
		ifTrue: [ SystemAnnouncer uniqueInstance classAdded: self inCategory: self category ]
		ifFalse: [ SystemAnnouncer uniqueInstance class: self recategorizedFrom: oldCategory to: self category ]
]

{ #category : 'subclass creation - variableByte' }
Class >> checkForCompiledMethodLayout: className [ 
	"There is no class definiton message for CompiledMethodLayout, it just uses the one for bytelayout for CompiledCode, CompiledMethod and CompiledBlock"

	| oldClassOrNil actualLayoutClass |
	oldClassOrNil := self environment at: className ifAbsent: [ nil ].
	actualLayoutClass := (oldClassOrNil notNil and: [ oldClassOrNil classLayout class == CompiledMethodLayout ])
		ifTrue: [ CompiledMethodLayout ]
		ifFalse: [ ByteLayout ].
	^ actualLayoutClass

]

{ #category : 'subclass creation' }
Class >> classBuilder [
		"Answer the object responsible of creating subclasses of myself in the system."

		^ self classInstaller new builder
]

{ #category : 'accessing - comment' }
Class >> classComment: aString [
	self
		deprecated: 'use comment:'
		transformWith: '`@rcv classComment: `@arg' -> '`@rcv comment: `@arg'.
	self comment: aString
]

{ #category : 'accessing - comment' }
Class >> classComment: aString stamp: aStamp [
		self
		deprecated: 'Use #comment:stamp: instead.'
		transformWith: '`@rcv classComment: `@arg1 classComment: aString stamp: aStamp `@arg2' -> '`@rcv comment: `@arg1 stamp: `@arg2'.
	^self comment: aString stamp: aStamp
]

{ #category : 'subclass creation' }
Class >> classInstaller [
	"Answer the class responsible of creating subclasses of myself in the system."
	^self isAnonymous
		ifTrue: [Smalltalk anonymousClassInstaller]
		ifFalse: [Smalltalk classInstaller]
]

{ #category : 'accessing' }
Class >> classPool [
	"Answer the dictionary of class variables.
	We initialize in basicDeclareClassVariable: to not allocate unused empty dictionaries"
	<reflection: 'Class structural inspection - Shared pool inspection'>
	^classPool ifNil: [ Dictionary new ]
]

{ #category : 'accessing' }
Class >> classPool: aDictionary [
	<reflection: 'Class structural modification - Shared pool modification'>
	classPool := aDictionary
]

{ #category : 'accessing - parallel hierarchy' }
Class >> classSide [
	"Return the metaclass of the couple class/metaclass. Useful to avoid explicit test."
	"Point classSide >>> Point class"
	"Point class classSide >>> Point class"
	<reflection: 'Class structural inspection - Class/Metaclass shift'>
	^ self class
]

{ #category : 'class variables' }
Class >> classVarNamed: aString [
	"for compatibility"

	^self readClassVariableNamed: aString
]

{ #category : 'class variables' }
Class >> classVarNamed: aString put: anObject [
	"for compatibility"

	self writeClassVariableNamed: aString value: anObject
]

{ #category : 'class variables' }
Class >> classVarNames [
	"Answer a collection of the receiver's class variable names."
	<reflection: 'Class structural inspection - Class variable inspection'>
	^self classPool keys sort
]

{ #category : 'class variables' }
Class >> classVariableDefinitionString [
	"Answer a string that evaluates to the definition of the class Variables"

	^String streamContents: [ :str | | fullDef |
		str nextPutAll: '{ '.
		self classVariables do: [:global |
				str nextPutAll: global definitionString.
				fullDef := global needsFullDefinition]
			separatedBy: [
				str nextPutAll: ' . '.
				fullDef ifTrue: [ str cr;tab;tab;tab;tab ]].
		str nextPutAll: ' }'. ]
]

{ #category : 'class variables' }
Class >> classVariableNamed: aString [
	"Answer the Class Variable"

	^self classVariableNamed: aString ifAbsent: [self error: 'no such class var']
]

{ #category : 'class variables' }
Class >> classVariableNamed: aString ifAbsent: absentBlock [
	"Answer the Class Variable"
	<reflection: 'Class structural inspection - Class variable inspection'>
	^self classPool associationAt: aString asSymbol ifAbsent: absentBlock
]

{ #category : 'class variables' }
Class >> classVariables [
	"Answer the meta objects of all class variables"
	<reflection: 'Class structural inspection - Class variable inspection'>
	^self classPool associations
]

{ #category : 'slots' }
Class >> classVariablesNeedFullDefinition [

	^ self classVariables anySatisfy: [ :each | each needsFullDefinition ]
]

{ #category : 'accessing - comment' }
Class >> comment [
    ^ self commentSourcePointer
		  ifNil: [ '' ]
		  ifNotNil: [ :ptr | SourceFiles sourceCodeAt: ptr ]
]

{ #category : 'accessing - comment' }
Class >> comment: aString [
	"Set the receiver's comment to be the argument, aStringOrText."
	^ self comment: aString stamp: '<historical>'
]

{ #category : 'accessing - comment' }
Class >> comment: aString stamp: aStamp [
	"Store the comment, aString or Text or RemoteString, associated with the class we are organizing.  Empty string gets stored only if had a non-empty one before."

	| pointer header oldComment oldStamp preamble |
	oldComment := self comment.
	oldStamp := self commentStamp.

	aString string = oldComment string ifTrue: [ ^ self ].

	pointer := self commentSourcePointer ifNil: [0].


	preamble := String streamContents: [ :file |
		file cr; nextPut: $!.
		header := String streamContents: [:strm |
			strm
				nextPutAll: self name;
				nextPutAll: ' commentStamp: '.
			aStamp storeOn: strm.
			strm nextPutAll: ' prior: '; nextPutAll: pointer printString ].
		file nextChunkPut: header; cr ].

	SourceFiles
		writeSource: aString
		preamble: preamble
		onSuccess: [ :newSourcePointer |
			self commentSourcePointer: newSourcePointer]
		onFail: [ "ignore" ].

	SystemAnnouncer uniqueInstance
		class: self
		oldComment: oldComment
		newComment: aString
		oldStamp: oldStamp
		newStamp: aStamp
]

{ #category : 'accessing - comment' }
Class >> commentSourcePointer [
	^ commentSourcePointer
]

{ #category : 'accessing - comment' }
Class >> commentSourcePointer: anObject [
	commentSourcePointer := anObject
]

{ #category : 'accessing - comment' }
Class >> commentStamp [

	^ self commentSourcePointer
		  ifNil: [ '' ]
		  ifNotNil: [:sourcePointer | SourceFiles commentTimeStampAt: sourcePointer ]
]

{ #category : 'accessing - comment' }
Class >> commentStamp: changeStamp [
	"update the changeStamp"
	self comment: self comment stamp: changeStamp
]

{ #category : 'accessing - class hierarchy' }
Class >> commonSuperclassWith: aClass [
	"return the next common superclass between me and aClass. If I am the superclass of aClass, that is me"
	<reflection: 'Class structural inspection - Iterating and querying hierarchy'>
	^ self withAllSuperclasses detect: [ :class | (aClass allSuperclasses includes: class) ] ifNone: nil
]

{ #category : 'compiling' }
Class >> compileAllFrom: oldClass [
	"Recompile all the methods in the receiver's method dictionary (not the
	subclasses). Also recompile the methods in the metaclass."

	super compileAllFrom: oldClass.
	self classSide compileAllFrom: oldClass classSide
]

{ #category : 'copying' }
Class >> copyForAnnouncement [
	"Answer a copy of the receiver to be used in the announcement of changes.
	You should not use this class for anything else, it is invalid."

	| newClass |
	newClass := self class copy new
		            basicSuperclass: superclass;
		            methodDict: self methodDict copy;
		            setFormat: format;
		            setName: name;
		            classPool: classPool copy;
		            sharedPools: sharedPools copy.
	self class instSize + 1 to: self class instSize do: [ :offset | newClass instVarAt: offset put: (self instVarAt: offset) ].
	newClass classLayout: (layout copy host: newClass).
	^ newClass
]

{ #category : 'initialization' }
Class >> declareClassVariables: newVars [
	"Declare class variables common to all instances. Answer whether
	recompilation is advisable."
	|  conflicts newVarNames |
	conflicts := false.
	newVarNames := newVars collect: [ :each | each name ].
	"Remove. Warn if vars are removed that are still used"
	(self classVarNames difference: newVarNames) do: [:varName |
		self removeClassVarNamed: varName interactive: false].

	(newVarNames difference: self classVarNames) do: [:varName | "adding"
			"check if new vars are defined elsewhere"
			(self innerBindingOf: varName) ifNotNil: [:existingVar |
				DuplicatedVariableError signalWith: existingVar.
				conflicts := true]].
	newVars do: [:var | self basicDeclareClassVariable: var ].
	^conflicts
]

{ #category : 'pool variables' }
Class >> definedVariables [
	"return all the Variables defined by this class"
	<reflection: 'Class structural inspection - Instance variable inspection'>
	^self slots, self classVariables
]

{ #category : 'class variables' }
Class >> definesClassVariable: aGlobal [
	"Return whether the receiver has a class variables (shared variables among its class and subclasses) named: aString"
	<reflection: 'Class structural inspection - Class variable inspection'>
	^ self classVariables includes: aGlobal
]

{ #category : 'class variables' }
Class >> definesClassVariableNamed: aString [
	"Return whether the receiver has a class variables (shared variables among its class and subclasses) named: aString"
	<reflection: 'Class structural inspection - Class variable inspection'>
	^ self classVarNames includes: aString
]

{ #category : 'fileout' }
Class >> definitionStringFor: aConfiguredPrinter [

	^ aConfiguredPrinter classDefinitionString
]

{ #category : 'deprecation' }
Class >> deprecationRefactorings [

	"Return a list of refactorings that will apply class deprecation in the system. E.g. to
	move installed extension methods to a proper class to which the deprecated class was renamed.
	For default deprecation use the following refactoring:
	^ { RBApplyClassDeprecationRefactoring rename: self to: #NewName } "

	^ #()
]

{ #category : 'copying' }
Class >> duplicateClassWithNewName: aSymbol [

	| copysName class |
	copysName := aSymbol asSymbol.
	copysName = self name ifTrue: [ ^ self ].
	(self environment includesKey: copysName) ifTrue: [ ^ self error: copysName , ' already exists' ].
	class := self classInstaller make: [ :builder |
		         builder
			         fillFor: self;
			         name: copysName ].
	class copyAllMethodsFrom: self.
	class class copyAllMethodsFrom: self class.
	^ class
]

{ #category : 'organization' }
Class >> environment [

	^environment ifNil: [super environment]
]

{ #category : 'organization' }
Class >> environment: anEnvironment [

	environment := anEnvironment
]

{ #category : 'subclass creation - weak' }
Class >> ephemeronSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames package: cat [
	"Added to allow for a simplified subclass creation experience. "

	 ^ self classInstaller
			make: [ :builder |
				builder
					superclass: self;
					name: className;
					layoutClass: EphemeronLayout;
					slotsFromString: instVarNameList;
					sharedVariablesFromString: classVarNames;
					category: cat;
					environment: self environment ]
]

{ #category : 'subclass creation - deprecated' }
Class >> ephemeronSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat [

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  layoutClass: EphemeronLayout;
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPools: s;
			  category: cat;
			  environment: self environment ]
]

{ #category : 'subclass creation - weak' }
Class >> ephemeronSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s package: cat [
	"This is the standard initialization message for creating a new class as a subclass of an existing class (the receiver) in which the subclass is to have weak indexable pointer variables."

	^ self classInstaller
		make: [ :builder |
			builder
				superclass: self;
				name: t;
				layoutClass: EphemeronLayout;
				slotsFromString: f;
				sharedVariablesFromString: d;
				sharedPools: s;
				category: cat;
				environment: self environment ]
]

{ #category : 'fileout' }
Class >> expandedDefinitionStringFor: aPrinter [

	^ aPrinter expandedDefinitionString
]

{ #category : 'private' }
Class >> getName [

	^ name
]

{ #category : 'testing' }
Class >> hasAbstractMethods [
	"Tells whether the receiver locally defines an abstract method, i.e., a method sending subclassResponsibility"
	<reflection: 'Class structural inspection - Selectors and methods inspection'>
	^ super hasAbstractMethods or: [ self classSide hasAbstractMethods ]
]

{ #category : 'compiling' }
Class >> hasBindingThatBeginsWith: aString [
	"Answer true if the receiver has a binding that begins with aString, false otherwise"

	"First check the slots / instance variabless"
	(self classLayout hasBindingThatBeginsWith: aString) ifTrue: [^ true].

	"Then look in classVar dictionary"
	(self classPool hasBindingThatBeginsWith: aString) ifTrue: [ ^true].

	"Next look in shared pools"
	(self sharedPools anySatisfy: [:pool | pool hasBindingThatBeginsWith: aString ]) ifTrue: [ ^true].

	"Last: go to the outer environment"
	^ self environment hasBindingThatBeginsWith: aString
]

{ #category : 'accessing - parallel hierarchy' }
Class >> hasClassSide [
	<reflection: 'Class structural inspection - Class/Metaclass shift'>
	^self classSide notNil
]

{ #category : 'class variables' }
Class >> hasClassVarNamed: aString [
	"Return whether the receiver has a class variables (shared variables among its class and subclasses) named: aString"
	<reflection: 'Class structural inspection - Class variable inspection'>
	^ self classPool includesKey: aString
]

{ #category : 'class variables' }
Class >> hasClassVariable: aGlobal [
	"Return whether the receiver has a class variables (shared variables among its class and subclasses) named: aString"
	<reflection: 'Class structural inspection - Class variable inspection'>
	^ self classVariables identityIncludes: aGlobal
]

{ #category : 'accessing - comment' }
Class >> hasComment [
	"Return whether this class truly has a comment other than the default"
	^ self commentSourcePointer isNotNil
]

{ #category : 'testing' }
Class >> hasMethods [
	"Answer a Boolean according to whether any methods are defined for the
	receiver (includes whether there are methods defined in the receiver's
	metaclass)."
	<reflection: 'Class structural inspection - Selectors and methods inspection'>
	^super hasMethods or: [ self classSide hasMethods ]
]

{ #category : 'pool variables' }
Class >> hasSharedPools [
	"Returns whether the receiver uses shared pools directly (Does not take into account that it may inherit shared pool uses."
	<reflection: 'Class structural inspection - Shared pool inspection'>
	^ self sharedPools notEmpty
]

{ #category : 'accessing - class hierarchy' }
Class >> hasSubclasses [
	<reflection: 'Class structural inspection - Iterating and querying hierarchy'>
	^ self subclasses isNotEmpty
]

{ #category : 'subclass creation - immediate' }
Class >> immediateSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames package: cat [
	"
	An immediate subclass define a class for which its value is coded within the OOP itself (like SmallInteger in 32 bits). It is not meant to be used by non-experimented users.

	Immediates are objects that are stored in an object pointer using a tag to distinguish them from ordinary object pointers.  In v3 the only immediate is SmallInteger.  In 32-but spur there are SmallInteger and Character.  An implication of this is that in spur all Characters can be compared using #==.  In 64-bit Spur there is also SmallFloat64.  If a float's exponent is in the middle 8-bits of the 11-bit exponent range then it will be immediate.  If a float's exponent is outside of the middle 8-bits it will be boxed.
	"
	^ self classInstaller make: [ :builder |
		builder
			superclass: self;
			name: className;
			layoutClass: ImmediateLayout;
			slotsFromString: instVarNameList;
			sharedVariablesFromString: classVarNames;
			category: cat;
			environment: self environment ]
]

{ #category : 'subclass creation - deprecated' }
Class >> immediateSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat [
	^ self classInstaller make: [ :builder |
		builder
			superclass: self;
			name: t;
			layoutClass: ImmediateLayout;
			slotsFromString: f;
			sharedVariablesFromString: d;
			sharedPools: s;
			category: cat;
			environment: self environment ]
]

{ #category : 'subclass creation - immediate' }
Class >> immediateSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s package: cat [
	"This is the standard initialization message for creating a new class as a
	subclass of an existing class (the receiver) in which the subclass is to
	have indexable pointer variables."

	^ self classInstaller make: [ :builder |
		builder
			superclass: self;
			name: t;
			layoutClass: ImmediateLayout;
			slotsFromString: f;
			sharedVariablesFromString: d;
			sharedPools: s;
			category: cat;
			environment: self environment ]
]

{ #category : 'compiling' }
Class >> innerBindingOf: aSymbol [
	"Answer the binding of some variable resolved in the scope of the receiver, or one of its superclass
	but do not look up binding in receiver's environment.
	Use #bindingOf: for looking up the variable binding in a full scope, including receiver's environment"

	"First look in classVar dictionary."
	(self classPool bindingOf: aSymbol) ifNotNil: [:binding | ^binding].
	"Next look in shared pools."
	self sharedPools do: [:pool | (pool bindingOf: aSymbol) ifNotNil: [:binding | ^binding]].
	self superclass ifNotNil: [:supercl | ^ supercl innerBindingOf: aSymbol].
	^ nil
]

{ #category : 'accessing - parallel hierarchy' }
Class >> instanceSide [
	"Return the class of the couple class/metaclass. Useful to avoid explicit test."
	"Point instanceSide >>> Point"
	"Point class instanceSide >>> Point"
	<reflection: 'Class structural inspection - Class/Metaclass shift'>
	^ self
]

{ #category : 'testing' }
Class >> isAnonymous [
	<reflection: 'Class structural inspection - Class kind testing'>
	^self getName isNil
]

{ #category : 'testing' }
Class >> isClass [
	<reflection: 'Class structural inspection - Class kind testing'>
	^ true
]

{ #category : 'testing' }
Class >> isClassOrTrait [
	<reflection: 'Class structural inspection - Class kind testing'>
	^true
]

{ #category : 'testing' }
Class >> isObsolete [
	"Return true if the receiver is obsolete."
	<reflection: 'Class structural inspection - Class kind testing'>
	^ ObsoleteClasses includes: self
]

{ #category : 'self evaluating' }
Class >> isSelfEvaluating [
	^self isObsolete not
]

{ #category : 'testing' }
Class >> isUsed [
	<reflection: 'Class structural inspection - Class kind testing'>
	^ self hasSubclasses
		ifFalse: [ super isUsed ]
		ifTrue: [ true ]
]

{ #category : 'accessing' }
Class >> name [
	"Answer the name of the receiver."

	^ name ifNil: [ super name ]
]

{ #category : 'slots' }
Class >> needsSlotClassDefinition [
    "return true if we define something else than InstanceVariableSlots or normal class variables"

    ^ super needsSlotClassDefinition or: [ self classVariablesNeedFullDefinition ]
]

{ #category : 'subclass creation' }
Class >> newAnonymousSubclass [
	<reflection: 'Class structural modification - Anonymous class creation'>
	^ Smalltalk anonymousClassInstaller make: [ :builder |
		builder
			superclass: self ;
			layoutClass: self classLayout class ]
]

{ #category : 'subclass creation' }
Class >> newSubclass [
	<reflection: 'Class structural modification - Hierarchy modification'>
	| i className |
	i := 1.
	[ className := (self name , i printString) asSymbol.
	self environment includesKey: className ] whileTrue: [ i := i + 1 ].
	^ self classInstaller
		make: [ :builder |
			builder
				superclass: self; 
				name: className;
				environment: self environment ]
]

{ #category : 'initialization' }
Class >> obsolete [
	"Change the receiver and all of its subclasses to an obsolete class."
	self == Object
		ifTrue: [^self error: 'Object is NOT obsolete'].
	self setName: 'AnObsolete' , self name.
	Object class instSize + 1 to: self classSide instSize do:
		[:i | self instVarAt: i put: nil]. "Store nil over class instVars."
	self classPool: nil.
	self sharedPools: nil.
	self hasClassSide ifTrue: [ self classSide obsolete].
	ObsoleteClasses add: self.
	super obsolete
]

{ #category : 'compiling' }
Class >> possibleVariablesFor: misspelled continuedFrom: oldResults [

	| results |
	results := misspelled correctAgainstDictionary: self classPool continuedFrom: oldResults.
	self sharedPools do: [:pool |
		results := misspelled correctAgainstDictionary: pool classPool continuedFrom: results ].
	^ self superclass
		ifNil: [ misspelled correctAgainstDictionary: self environment continuedFrom: results ]
		ifNotNil: [ self superclass possibleVariablesFor: misspelled continuedFrom: results ]
]

{ #category : 'class variables' }
Class >> readClassVariableNamed: aString [
	"Answer the content of the Class Variable"
	<reflection: 'Class structural inspection - Class variable inspection'>
	^(self classVariableNamed: aString) read
]

{ #category : 'compiling' }
Class >> reformatAll [
	"Reformat all methods in this class.
	Leaves old code accessible to version browsing"

	super reformatAll.		"me..."
	self classSide reformatAll.	"...and my metaclass"
]

{ #category : 'class variables' }
Class >> removeClassVarNamed: aString [
	"Remove the class variable whose name is the argument, aString, from
    the names defined in the receiver, a class. Create an error notification if
    aString is not a class variable or if it is still being used in the code of
    the class."
	<reflection: 'Class structural modification - Class variable modification'>
	self removeClassVarNamed: aString interactive: false
]

{ #category : 'class variables' }
Class >> removeClassVarNamed: aString interactive: interactive [
	"Remove the class variable whose name is the argument, aString, from
    the names defined in the receiver, a class. Create an error notification if
    aString is not a class variable or if it is still being used in the code of
    the class."
	<reflection: 'Class structural modification - Class variable modification'>

	| aSymbol varUserClass |
	aSymbol := aString asSymbol.
	(self classPool includesKey: aSymbol) ifFalse: [ ^ self error: aString , ' is not a class variable' ].
	varUserClass := self anyUserOfClassVarNamed: aSymbol.
	varUserClass notNil & interactive ifTrue: [
		(self confirm: (aString , ' is still used in code of class ' , varUserClass name , '.\Is it okay to move it to Undeclared?') withCRs) ifFalse: [ ^ self ] ].

	varUserClass
		ifNotNil: [
			NewUndeclaredWarning signal: aString in: self name.
			Undeclared declare: aSymbol from: self classPool ]
		ifNil: [ self classPool removeKey: aSymbol ].

	self classPool ifEmpty: [ self classPool: nil ].

	SystemAnnouncer uniqueInstance classModificationAppliedTo: self
]

{ #category : 'class variables' }
Class >> removeClassVariable: aGlobal [
	"Remove the class variable"
	<reflection: 'Class structural modification - Class variable modification'>
	self removeClassVarNamed: aGlobal name
]

{ #category : 'initialization' }
Class >> removeFromSystem [
	"Forget the receiver from the Smalltalk global dictionary. Any existing
	instances will refer to an obsolete version of the receiver."
	self removeFromSystem: true
]

{ #category : 'initialization' }
Class >> removeFromSystem: logged [
	"Forget the receiver from the Smalltalk global dictionary. Any existing  instances will refer to an obsolete version of the receiver.
	Keep the class name and category for triggering the system change message. If we wait to long, then we get obsolete information which is not what we want.
	Tell class to deactivate and unload itself-- two separate events in the module system"

	| myCategory package |
	self release.
	self unload.

	self superclass ifNotNil: [ "If we have no superclass there's nothing to be remembered" self superclass addObsoleteSubclass: self ].

	"we add the class to Undeclared so that if references still exist, they will  be automatically fixed
	if this class is loaded again. We do not check if references exist as it is too slow"
	Undeclared declare: self name asSymbol from: Smalltalk globals.
	myCategory := self category.
	package := self package.
	self environment forgetClass: self.

	"In case the class has deprecated aliases we need to remove them from the system dictionary.
	We deal also with a special case that is the case a class of the same name than the alias is installed in the image after the alias. In that case we do not remove it. It's the reason we check if the global of the alias is the same as self."
	self deprecatedAliases do: [ :alias | self environment at: alias ifPresent: [ :class | class = self ifTrue: [ self environment removeKey: alias ] ] ].
	self obsolete.
	logged ifTrue: [ SystemAnnouncer announce: (ClassRemoved class: self package: package category: myCategory) ]
]

{ #category : 'initialization' }
Class >> removeFromSystemUnlogged [
	"Forget the receiver from the Smalltalk global dictionary. Any existing instances will refer to an obsolete version of the receiver.  Do not log the removal either to the current change set nor to the system changes log"
	^self removeFromSystem: false
]

{ #category : 'pool variables' }
Class >> removeSharedPool: aDictionary [
	"Remove the pool dictionary, aDictionary, as one of the receiver's pool
	dictionaries. Create an error notification if the dictionary is not one of
	the pools.
	: Note that it removes the wrong one if there are two empty Dictionaries in the list."
	<reflection: 'Class structural modification - Shared pool modification'>

	| satisfiedSet workingSet aSubclass |
	(self sharedPools includes: aDictionary) ifFalse: [ ^ self error: 'the dictionary is not in my pool' ].

	"first see if it is declared in a superclass in which case we can remove it."
	(self selectSuperclasses: [ :class | class sharedPools includes: aDictionary ]) ifNotEmpty: [
		self sharedPools remove: aDictionary.
		self sharedPools isEmpty ifTrue: [ self sharedPools: nil ].
		^ self ].

	"second get all the subclasses that reference aDictionary through me rather than a
	superclass that is one of my subclasses."

	workingSet := self subclasses asOrderedCollection.
	satisfiedSet := Set new.
	[ workingSet isEmpty ] whileFalse: [
		aSubclass := workingSet removeFirst.
		(aSubclass sharedPools includes: aDictionary) ifFalse: [
			satisfiedSet add: aSubclass.
			workingSet addAll: aSubclass subclasses ] ].

	"for each of these, see if they refer to any of the variables in aDictionary because
	if they do, we can not remove the dictionary."
	satisfiedSet add: self.
	satisfiedSet do: [ :sub |
		aDictionary associationsDo: [ :aGlobal |
			(sub whichMethodsReferTo: aGlobal) ifNotEmpty: [ ^ self error: aGlobal key , ' is still used in code of class ' , sub name ] ] ].
	self sharedPools remove: aDictionary.
	self sharedPools ifEmpty: [ self sharedPools: nil ]
]

{ #category : 'accessing - class hierarchy' }
Class >> removeSubclass: aSubclass [
	"If the argument, aSubclass, is one of the receiver's subclasses, remove it."
	<reflection: 'Class structural modification - Hierarchy modification'>
	self subclasses == nil ifFalse: [
		self subclasses: (self subclasses copyWithout: aSubclass).
		self subclasses ifEmpty: [ self subclasses: nil ] ]
]

{ #category : 'class name' }
Class >> rename: aString [
	"The new name of the receiver is the argument, aString."

	| oldName newName |
	(newName := aString asSymbol) = (oldName := self name)
		ifTrue: [^ self].
	(self environment includesKey: newName)
		ifTrue: [^ self error: newName , ' already exists'].
	self setName: newName.
	self environment renameClass: self from: oldName.
	(Undeclared includesKey: newName)
		ifTrue: [self inform: 'There are references to, ' , aString printString , '
from Undeclared. Check them after this change.']
]

{ #category : 'private' }
Class >> setName: aSymbol [

	name := aSymbol
]

{ #category : 'pool variables' }
Class >> sharedPoolNames [
	<reflection: 'Class structural inspection - Shared pool inspection'>
	^ self sharedPools collect: [:ea |
		ea isObsolete
			ifTrue: [ ea name ]
			ifFalse: [ self environment keyAtIdentityValue: ea ] ]
]

{ #category : 'pool variables' }
Class >> sharedPoolOfVarNamed: aString [
	"Returns the SharedPool or nil from which the pool variable named aString is coming from."
	<reflection: 'Class structural inspection - Shared pool inspection'>
	^ self sharedPools
		detect: [ :each | each usesClassVarNamed: aString ]
		ifNone: [ self superclass ifNotNil: [ self superclass sharedPoolOfVarNamed: aString ] ]
]

{ #category : 'pool variables' }
Class >> sharedPools [
	"Answer an orderedCollection of the shared pools declared in the receiver.
	Note: To save memory, the variable is initialized by #addSharedPool:"
	<reflection: 'Class structural inspection - Shared pool inspection'>
	^ sharedPools ifNil: [ sharedPools := super sharedPools ]
]

{ #category : 'pool variables' }
Class >> sharedPools: aCollection [
	<reflection: 'Class structural modification - Shared pool modification'>
	sharedPools := aCollection
]

{ #category : 'pool variables' }
Class >> sharedPoolsDo: aBlockClosure [
	"Iterate shared pools.
	The shared pool collection is lazily created on access.
	This method avoids creating the collection for a simple iteration"
	<reflection: 'Class structural inspection - Shared pool inspection'>
	self hasSharedPools ifFalse: [ ^ self ].
	self sharedPools do: aBlockClosure
]

{ #category : 'initialization' }
Class >> sharing: poolString [
	"Set up sharedPools. Answer whether recompilation is advisable."

	| oldPools |
	oldPools := self sharedPools.
	self sharedPools: OrderedCollection new.
	(poolString substrings: ' ') do: [ :poolName |
		self sharedPools add: (self environment at: poolName asSymbol ifAbsent: [
				 (self confirm: 'The pool dictionary ' , poolName , ' does not exist.' , '\Do you want it automatically created?' withCRs)
					 ifTrue: [
						 self classInstaller make: [ :builder |
							 builder
								 superclass: SharedPool;
								 name: poolName;
								 category: self category ] ]
					 ifFalse: [ ^ self inform: poolName , ' does not exist' ] ]) ].
	self sharedPools ifEmpty: [ self sharedPools: nil ].
	oldPools do: [ :pool |
		| found |
		found := self sharedPools anySatisfy: [ :p | p == pool ].
		found ifFalse: [ ^ true "A pool got deleted" ] ].
	^ false
]

{ #category : 'subclass creation' }
Class >> subclass: t [
	<reflection: 'Class structural modification - Old class creation'>
	^ self classInstaller
		make: [ :builder |
			builder
				superclass: self; 
				name: t;
				environment: self environment ]

	
]

{ #category : 'subclass creation' }
Class >> subclass: t instanceVariableNames: ins [
	<reflection: 'Class structural modification - Old class creation'>
	^ self classInstaller
		make: [ :builder |
			builder
				superclass: self; 
				name: t;
				slotsFromString: ins;
				environment: self environment ]
]

{ #category : 'subclass creation - deprecated' }
Class >> subclass: aSubclassSymbol instanceVariableNames: instVarNameList classVariableNames: classVarNames category: aCategorySymbol [
	"Added to allow for a simplified subclass creation experience. "
	<reflection: 'Class structural modification - Old class creation'>

		^ self classInstaller
		make: [ :builder |
			builder
				superclass: self; 
				name: aSubclassSymbol;
				sharedVariablesFromString: classVarNames;
				slotsFromString: instVarNameList;
				environment: self environment ]
]

{ #category : 'subclass creation' }
Class >> subclass: aSubclassSymbol instanceVariableNames: instVarNameList classVariableNames: classVarNames package: aPackageSymbol [
	"Added to allow for a simplified subclass creation experience. "
	<reflection: 'Class structural modification - Old class creation'>

	^ self classInstaller
		make: [ :builder |
			builder
				superclass: self;
				name: aSubclassSymbol;
				slotsFromString: instVarNameList;
				sharedVariablesFromString: classVarNames;
				category: aPackageSymbol;
				environment: self environment ]
]

{ #category : 'subclass creation - deprecated' }
Class >> subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat [
	<reflection: 'Class structural modification - Old class creation'>

	^ self classInstaller
		make: [ :builder |
			builder
				superclass: self;
				name: t;
				slotsFromString: f;
				sharedVariablesFromString: d;
				sharedPools: s;
				category: cat;
				environment: self environment ]
]

{ #category : 'subclass creation' }
Class >> subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s package: cat [
	<reflection: 'Class structural modification - Old class creation'>

	^ self classInstaller
		make: [ :builder |
			builder
				superclass: self;
				name: t;
				layoutClass: self classLayout class;
				slotsFromString: f;
				sharedVariablesFromString: d;
				sharedPools: s;
				category: cat;
				environment: self environment ]
]

{ #category : 'subclass creation - slots' }
Class >> subclass: aSubclassSymbol layout: layoutClass slots: slotDefinition classVariables: classVarDefinition package: aCategorySymbol [
	<reflection: 'Class structural modification - Old class creation'>

	^ self classInstaller make: [ :builder |
		  builder
			  name: aSubclassSymbol;
			  superclass: self;
			  layoutClass: layoutClass;
			  slots: slotDefinition;
			  sharedVariables: classVarDefinition;
			  category: aCategorySymbol ]
]

{ #category : 'subclass creation - slots' }
Class >> subclass: aSubclassSymbol layout: layoutClass slots: slotDefinition classVariables: classVarDefinition poolDictionaries: someSharedPoolNames package: aCategorySymbol [
	<reflection: 'Class structural modification - Old class creation'>

	^ self classInstaller make: [ :builder |
		  builder
			  name: aSubclassSymbol;
			  superclass: self;
			  layoutClass: layoutClass;
			  slots: slotDefinition;
			  sharedVariables: classVarDefinition;
			  sharedPools: someSharedPoolNames;
			  category: aCategorySymbol ]
]

{ #category : 'subclass creation - slots' }
Class >> subclass: aSubclassSymbol slots: slotDefinition classVariables: classVarDefinition package: aCategorySymbol [
	<reflection: 'Class structural modification - Old class creation'>

	^ self classInstaller make: [ :builder |
		  builder
			  name: aSubclassSymbol;
			  superclass: self;
			  slots: slotDefinition;
			  sharedVariables: classVarDefinition;
			  category: aCategorySymbol ]
]

{ #category : 'subclass creation - slots' }
Class >> subclass: aSubclassSymbol slots: slotDefinition classVariables: classVarDefinition poolDictionaries: someSharedPoolNames package: aCategorySymbol [
	<reflection: 'Class structural modification - Old class creation'>

	^ self classInstaller make: [ :builder |
		  builder
			  name: aSubclassSymbol;
			  superclass: self;
			  slots: slotDefinition;
			  sharedVariables: classVarDefinition;
			  sharedPools: someSharedPoolNames;
			  category: aCategorySymbol ]
]

{ #category : 'accessing - class hierarchy' }
Class >> subclasses [
	"Answer a Set containing the receiver's subclasses."
	<reflection: 'Class structural inspection - Iterating and querying hierarchy'>
	^subclasses
		ifNil: [ #() ]
		ifNotNil: [ subclasses copy ]
]

{ #category : 'accessing - class hierarchy' }
Class >> subclasses: aCollection [
	<reflection: 'Class structural modification - Hierarchy modification'>
	subclasses := aCollection
]

{ #category : 'accessing - class hierarchy' }
Class >> subclassesDo: aBlock [
	"Evaluate the argument, aBlock, for each of the receiver's immediate subclasses."
	<reflection: 'Class structural inspection - Iterating and querying hierarchy'>
	self subclasses do: aBlock
]

{ #category : 'initialization' }
Class >> superclass: aClass methodDictionary: mDict format: fmt [
	"Basic initialization of the receiver"
	super superclass: aClass methodDictionary: mDict format: fmt.
	self subclasses: nil
]

{ #category : 'initialization' }
Class >> unload [
	"Sent when a the class is removed.  Does nothing, but may be overridden by (class-side) subclasses."
]

{ #category : 'class variables' }
Class >> usesClassVarNamed: aString [
	"Return whether the receiver or its superclasses have a class variable named: aString"
	<reflection: 'Class structural inspection - Class variable inspection'>
	^ self allClassVarNames includes: aString
]

{ #category : 'pool variables' }
Class >> usesLocalPoolVarNamed: aString [
	"Return whether the receiver uses a pool variable named: aString which is defined locally"
	<reflection: 'Class structural inspection - Shared pool inspection'>
	^ self sharedPools anySatisfy: [ :each | each usesClassVarNamed: aString ]
]

{ #category : 'pool variables' }
Class >> usesPoolVarNamed: aString [
	"Return whether the receiver has a pool variable named: aString, taking into account superclasses too"
	<reflection: 'Class structural inspection - Shared pool inspection'>
	^self allSharedPools anySatisfy: [:each | each usesClassVarNamed: aString]
]

{ #category : 'subclass creation - deprecated' }
Class >> variableByteSubclass: className instanceVariableNames: instVarNameList  classVariableNames: classVarNames category: cat [

	^ self classInstaller
		make: [ :builder |
			builder
				superclass: self;
				name: className;
				layoutClass: (self checkForCompiledMethodLayout: className);
				slotsFromString: instVarNameList;
				sharedVariablesFromString: classVarNames;
				category: cat;
				environment: self environment ]
]

{ #category : 'subclass creation - variableByte' }
Class >> variableByteSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames package: cat [
	"Added to allow for a simplified subclass creation experience. "

	^ self classInstaller
		make: [ :builder |
			builder
				superclass: self;
				name: className;
				layoutClass: (self checkForCompiledMethodLayout: className);
				slotsFromString: instVarNameList;
				sharedVariablesFromString: classVarNames;
				category: cat;
				environment: self environment ]
]

{ #category : 'subclass creation - deprecated' }
Class >> variableByteSubclass: className instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat [

	^ self classInstaller
		make: [ :builder |
			builder
				superclass: self;
				name: className;
				layoutClass: (self checkForCompiledMethodLayout: className);
				slotsFromString: f;
				sharedVariablesFromString: d;
				sharedPools: s;
				category: cat;
				environment: self environment ]
]

{ #category : 'subclass creation - variableByte' }
Class >> variableByteSubclass: className instanceVariableNames: f classVariableNames: d poolDictionaries: s package: cat [
	"This is the standard initialization message for creating a new class as a
	subclass of an existing class (the receiver) in which the subclass is to
	have indexable byte-sized nonpointer variables."
	
	^ self classInstaller
		make: [ :builder |
			builder
				superclass: self;
				name: className;
				layoutClass: (self checkForCompiledMethodLayout: className);
				slotsFromString: f;
				sharedVariablesFromString: d;
				sharedPools: s;
				category: cat;
				environment: self environment ]
]

{ #category : 'subclass creation - variableByte' }
Class >> variableDoubleByteSubclass: className instanceVariableNames: instVarNameList  classVariableNames: classVarNames package: cat [

	^ self classInstaller
		make: [ :builder |
			builder
				superclass: self;
				name: className;
				layoutClass: DoubleByteLayout;
				slotsFromString: instVarNameList;
				sharedVariablesFromString: classVarNames;
				category: cat;
				environment: self environment ]
]

{ #category : 'subclass creation - variableByte' }
Class >> variableDoubleWordSubclass: className instanceVariableNames: instVarNameList  classVariableNames: classVarNames package: cat [

	^ self classInstaller
		make: [ :builder |
			builder
				superclass: self;
				name: className;
				layoutClass: DoubleWordLayout;
				slotsFromString: instVarNameList;
				sharedVariablesFromString: classVarNames;
				category: cat;
				environment: self environment ]
]

{ #category : 'subclass creation - deprecated' }
Class >> variableSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames category: cat [

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: VariableLayout;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  category: cat;
			  environment: self environment ]
]

{ #category : 'subclass creation - variable' }
Class >> variableSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames package: cat [

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: VariableLayout;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  category: cat;
			  environment: self environment ]
]

{ #category : 'subclass creation - deprecated' }
Class >> variableSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames poolDictionaries: s category: cat [

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: VariableLayout;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  sharedPools: s;
			  category: cat;
			  environment: self environment ]
]

{ #category : 'subclass creation - variable' }
Class >> variableSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s package: cat [
	"This is the standard initialization message for creating a new class as a
	subclass of an existing class (the receiver) in which the subclass is to
	have indexable pointer variables."

	^ self classInstaller
		make: [ :builder |
			builder
				superclass: self;
				name: t;
				layoutClass: VariableLayout;
				slotsFromString: f;
				sharedVariablesFromString: d;
				sharedPools: s;
				category: cat;
				environment: self environment ]
]

{ #category : 'subclass creation - deprecated' }
Class >> variableWordSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames category: cat [
	"Added to allow for a simplified subclass creation experience. "

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: WordLayout;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  category: cat;
			  environment: self environment ]
]

{ #category : 'subclass creation - variableWord' }
Class >> variableWordSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames package: cat [
	"Variable word is like variable byte (ByteArray), variable size, with indices instead of named instance variables, but each index points to a full word (32 bits).

	Objects on the heap are either pointers or bits.  For example instances of Point, Array, BlockClosure etc are pointer objects.  But Bitmap, ByteString, WideString etc are bits objects.  In v3 bits objects are either a sequence of bytes (ByteArray, ByteString, ByteSymbol etc) or 32-bit words (WideString, Float, Bitmap etc).  Spur supports byte, short, word and double-word bits objects even though currently only byte and word classes exist.  16-bit strings will be useful on Windows, for example."

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: WordLayout;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  category: cat;
			  environment: self environment ]
]

{ #category : 'subclass creation - deprecated' }
Class >> variableWordSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat [

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  layoutClass: WordLayout;
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPools: s;
			  category: cat;
			  environment: self environment ]
]

{ #category : 'subclass creation - variableWord' }
Class >> variableWordSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s package: cat [
	"This is the standard initialization message for creating a new class as a subclass of an existing class (the receiver) in which the subclass is to have indexable word-sized nonpointer variables."

	^ self classInstaller
		make: [ :builder |
			builder
				superclass: self;
				name: t;
				layoutClass: WordLayout;
				slotsFromString: f;
				sharedVariablesFromString: d;
				sharedPools: s;
				category: cat;
				environment: self environment ]
]

{ #category : 'subclass creation - deprecated' }
Class >> weakSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames category: cat [
	"Added to allow for a simplified subclass creation experience. "

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: WeakLayout;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  category: cat;
			  environment: self environment ]
]

{ #category : 'subclass creation - weak' }
Class >> weakSubclass: className instanceVariableNames: instVarNameList classVariableNames: classVarNames package: cat [
	"Added to allow for a simplified subclass creation experience. "

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: className;
			  layoutClass: WeakLayout;
			  slotsFromString: instVarNameList;
			  sharedVariablesFromString: classVarNames;
			  category: cat;
			  environment: self environment ]
]

{ #category : 'subclass creation - deprecated' }
Class >> weakSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat [

	^ self classInstaller make: [ :builder |
		  builder
			  superclass: self;
			  name: t;
			  layoutClass: WeakLayout;
			  slotsFromString: f;
			  sharedVariablesFromString: d;
			  sharedPools: s;
			  category: cat;
			  environment: self environment ]
]

{ #category : 'subclass creation - weak' }
Class >> weakSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s package: cat [
	"This is the standard initialization message for creating a new class as a subclass of an existing class (the receiver) in which the subclass is to have weak indexable pointer variables."

	^ self classInstaller
		make: [ :builder |
			builder
				superclass: self;
				name: t;
				layoutClass: WeakLayout;
				slotsFromString: f;
				sharedVariablesFromString: d;
				sharedPools: s;
				category: cat;
				environment: self environment ]
]

{ #category : 'class variables' }
Class >> writeClassVariableNamed: aString value: anObject [
	"Store anObject in the class variable."
	<reflection: 'Class structural modification - Slot modification'>
	(self classVariableNamed: aString) write: anObject
]
