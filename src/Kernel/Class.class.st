"
I add a number of facilities to those in ClassDescription:
	A set of all my subclasses (defined in ClassDescription, but only used here and below)
	A name by which I can be found in a SystemDictionary
	A classPool for class variables shared between this class and its metaclass
	A list of sharedPools which probably should be supplanted by some better mechanism.

My instances describe the representation and behavior of objects. I add more comprehensive programming support facilities to the basic attributes of Behavior and the descriptive facilities of ClassDescription.

The slot 'subclasses' is a redundant structure.  It is never used during execution, but is used by the development system to simplify or speed certain operations.  
"
Class {
	#name : #Class,
	#superclass : #ClassDescription,
	#instVars : [
		'subclasses',
		'name',
		'classPool',
		'sharedPools',
		'environment',
		'category',
		'traitComposition',
		'localSelectors'
	],
	#category : #'Kernel-Classes'
}

{ #category : #'fileIn/Out' }
Class class >> allSuperclassesFor: aClass cache: cache [ 	
	^ cache at: aClass ifAbsentPut: [aClass allSuperclasses asArray]
]

{ #category : #'fileIn/Out' }
Class class >> doesNotIncludeInstanceOrSuperclassesFor: aClass in: unprocessedClasses cache: cache [ 
	| soleInstance |
	soleInstance := aClass soleInstance.
	^ (unprocessedClasses includes: soleInstance) not and: [
				self hasNoSuperclassesOf: soleInstance in: unprocessedClasses cache: cache]
]

{ #category : #private }
Class class >> hasNoDependenciesFor: aClass in: unprocessedClasses cache: cache [ 
	^ (self hasNoSuperclassesOf: aClass in: unprocessedClasses cache: cache) and: [
		aClass isMeta not or: [
			self hasNoDependenciesForMetaclass: aClass in: unprocessedClasses cache: cache]] 
]

{ #category : #'fileIn/Out' }
Class class >> hasNoDependenciesForMetaclass: aClass in: unprocessedClasses cache: cache [ 
	| soleInstance |
	soleInstance := aClass soleInstance.
	^ (unprocessedClasses includes: soleInstance) not and: [
				self hasNoSuperclassesOf: soleInstance in: unprocessedClasses cache: cache]
]

{ #category : #'fileIn/Out' }
Class class >> hasNoSuperclassesOf: aClass in: unprocessedClasses cache: cache [ 
	^ (unprocessedClasses includesAnyOf: (self allSuperclassesFor: aClass cache: cache)) not
	
]

{ #category : #inquiries }
Class class >> rootsOfTheWorld [
	"return all classes that have a nil superclass"
	
	^(self environment select: [:each | each isBehavior and: [each superclass isNil]]) asOrderedCollection
]

{ #category : #'fileIn/Out' }
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

{ #category : #'instance creation' }
Class class >> template: aSystemCategoryName [ 
	"Answer an expression that can be edited and evaluated in order to define a new class."

	^ self templateForSubclassOf: Object name category: aSystemCategoryName 
]

{ #category : #'instance creation' }
Class class >> templateForSubclassOf: priorClassName category: systemCategoryName [ 
	"Answer an expression that can be edited and evaluated in order to define a new class, given that the class previously looked at was as given"

	^priorClassName asString, ' subclass: #NameOfSubclass
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	package: ''' , systemCategoryName asString , ''''
]

{ #category : #'class variables' }
Class >> addClassVarNamed: aString [ 
	"Add the argument, aString, as a class variable of the receiver.
	Signal an error if the first character of aString is not capitalized,
	or if it is already a variable named in the class."
	
	self addClassVariable: (aString asSymbol => ClassVariable)
]

{ #category : #'class variables' }
Class >> addClassVariable: aGlobal [
	"Add the argument, aString, as a class variable of the receiver.
	Signal an error if the first character of aString is not capitalized,
	or if it is already a variable named in the class."
	| symbol oldState |
	oldState := self copy.
	symbol := aGlobal name asSymbol.
	self withAllSubclasses do: 
		[:subclass | 
		(subclass bindingOf: symbol) ifNotNil:[
			^ self error: symbol asString 
				, ' is already used as a variable name in class ' 
				, subclass name]].
	(self classPool includesKey: symbol) ifFalse: 
		["Pick up any refs in Undeclared"
		self classPool declareVariable: aGlobal from: Undeclared.
		SystemAnnouncer uniqueInstance 
			classDefinitionChangedFrom: oldState to: self;
			classModificationAppliedTo: self]
]

{ #category : #'instance variables' }
Class >> addInstVarNamed: aString [
	"Add the argument, aString, as one of the receiver's instance variables."

	<localClassMethod> 
    "This method is supposed to be local in Class because of a good reason.
    We use this pragma to test if Class does not contain some accidental 
    local selectors."

	aString substrings do: [ :each|
		self addSlot: (InstanceVariableSlot named: each asSymbol)]

]

{ #category : #'pool variables' }
Class >> addSharedPool: aSharedPool [ 
	"Add the argument, aSharedPool, as one of the receiver's shared pools. 
	Create an error if the shared pool is already one of the pools.
	This method will work with shared pools that are plain Dictionaries or thenewer SharedPool subclasses"

	(self sharedPools includes: aSharedPool)
		ifTrue: [^self error: 'This is already in my shared pool list'].
	self sharedPools == nil
		ifTrue: [self sharedPools: (OrderedCollection with: aSharedPool)]
		ifFalse: [self sharedPools add: aSharedPool]
]

{ #category : #'accessing class hierarchy' }
Class >> addSubclass: aSubclass [ 
	"Make the argument, aSubclass, be one of the subclasses of the receiver. 
	Create an error notification if the argument's superclass is not the receiver."
	aSubclass superclass ~~ self 
		ifTrue: [^self error: aSubclass name , ' is not my subclass'].
	subclasses ifNil: [
		self subclasses: (Array with: aSubclass).
		^ self ].
	self subclasses do:[:cl| cl == aSubclass ifTrue:[^self]]. "Already my subclass"
	self subclasses: (subclasses copyWith: aSubclass).
]

{ #category : #'class variables' }
Class >> allClassVarNames [
	"Answer a Set of the names of the receiver's class variables, including those
	defined in the superclasses of the receiver."

	| aSet |
	self superclass == nil
		ifTrue: 
			[^self classVarNames asSet]  "This is the keys so it is a new Set."
		ifFalse: 
			[aSet := self superclass allClassVarNames.
			aSet addAll: self classVarNames.
			^aSet]
]

{ #category : #'pool variables' }
Class >> allSharedPools [
	"Answer an ordered collection of the pools the receiver shares, including those defined  in the superclasses of the receiver."
	| aSet | 
	^self superclass == nil
		ifTrue: [self sharedPools copy]
		ifFalse: [aSet := self superclass allSharedPools.
			aSet addAll: self sharedPools.
			aSet]
]

{ #category : #'class variables' }
Class >> anyUserOfClassVarNamed: aSymbol [
	self withAllSubclasses do: [ :subclass | 
 		(Array with: subclass with: subclass class) do: [ :classOrMeta | 
			(classOrMeta whichSelectorsReferTo: (self classPool associationAt: aSymbol)) 
				ifNotEmpty: [ ^classOrMeta ]]].
				
	^nil
]

{ #category : #traits }
Class >> applyChangesOfNewTraitCompositionReplacing: oldComposition [

	| changedSelectors |
	changedSelectors := super applyChangesOfNewTraitCompositionReplacing: oldComposition.
	self classSide noteNewBaseTraitCompositionApplied: self traitComposition.
	^ changedSelectors
]

{ #category : #'accessing parallel hierarchy' }
Class >> baseClass [

	<localClassMethod> 
    "This method is supposed to be local in Class because of a good reason.
    We use this pragma to test if Class does not contain some accidental 
    local selectors."

	^ self
]

{ #category : #accessing }
Class >> basicCategory [
	^category
]

{ #category : #accessing }
Class >> basicCategory: aSymbol [
	category := aSymbol
]

{ #category : #accessing }
Class >> basicLocalSelectors [
	"Direct accessor for the instance variable localSelectors.
	Since localSelectors is lazily initialized, this may 
	return nil, which means that all selectors are local."

	<localClassMethod> 
    "This method is supposed to be local in Class because of a good reason.
    We use this pragma to test if Class does not contain some accidental 
    local selectors."

	^ localSelectors
]

{ #category : #accessing }
Class >> basicLocalSelectors: aSetOrNil [

	<localClassMethod> 
    "This method is supposed to be local in Class because of a good reason.
    We use this pragma to test if Class does not contain some accidental 
    local selectors."

	localSelectors := aSetOrNil
]

{ #category : #compiling }
Class >> binding [
       "Answer a binding for the receiver, sharing if possible"
      | binding |
	binding := self environment associationAt: self name ifAbsent: [nil -> self].
       ^binding value == self ifTrue: [binding] ifFalse: [nil -> self]
]

{ #category : #compiling }
Class >> bindingOf: varName [
	"Answer the binding of some variable resolved in the scope of the receiver, or nil
	if variable with such name is not defined"

	"The lookup recurses up to superclasses looking inside their class and shared pools,
	but not the environment, since two classes, even if they have ancestry relationship,
	could use different environments.
	That's why we doing an environment lookup only as a last step of symbol lookup 
	and taking only the environment of receiver only, not any of it's superclass(es) "
	
	| aSymbol |
	aSymbol := varName asSymbol.

	^ (self innerBindingOf: aSymbol) ifNil: [
		 self environment bindingOf: aSymbol
	]
]

{ #category : #organization }
Class >> category [
	"Answer the system organization category for the receiver. First check whether the
	category name stored in the ivar is still correct and only if this fails look it up
	(latter is much more expensive)"

	| result |
	self basicCategory ifNotNil: [ :symbol |
		((self environment organization listAtCategoryNamed: symbol) includes: self name)
			ifTrue: [ ^symbol ] ].
	result := (self environment organization categoryOfElement: self name)
		ifNil: [ #Unclassified ]
		ifNotNil: #yourself.
	self basicCategory: result.
	
	^ result
]

{ #category : #organization }
Class >> category: aString [ 
	"Categorize the receiver under the system category, aString, removing it from 
	any previous categorization."

	| oldCategory |
	oldCategory := self basicCategory.
	aString isString
		ifTrue: [
			self basicCategory: aString asSymbol.
			self environment organization classify: self name under: self basicCategory ]
		ifFalse: [self errorCategoryName].
	SystemAnnouncer uniqueInstance
		class: self recategorizedFrom: oldCategory to: self basicCategory
]

{ #category : #'subclass creation' }
Class >> classBuilder [
		"Answer the object responsible of creating subclasses of myself in the system."
	
		^ self classInstaller new builder
]

{ #category : #'accessing parallel hierarchy' }
Class >> classClass [

	<localClassMethod> 
    "This method is supposed to be local in Class because of a good reason.
    We use this pragma to test if Class does not contain some accidental 
    local selectors."

	^self class.
]

{ #category : #'subclass creation' }
Class >> classInstaller [
	"Answer the class responsible of creating subclasses of myself in the system."
	^self isAnonymous 
		ifTrue: [Smalltalk anonymousClassInstaller]
		ifFalse: [Smalltalk classInstaller]
]

{ #category : #accessing }
Class >> classPool [
	"Answer the dictionary of class variables."

	classPool ifNil: [ classPool := Dictionary new ].
		
	^ classPool
]

{ #category : #accessing }
Class >> classPool: aDictionary [
	classPool := aDictionary
]

{ #category : #'class variables' }
Class >> classVarNamed: aString [
	"for compatibility"

	^self readClassVariableNamed: aString
]

{ #category : #'class variables' }
Class >> classVarNamed: aString put: anObject [
	"for compatibility"
	
	self writeClassVariableNamed: aString value: anObject
]

{ #category : #'class variables' }
Class >> classVarNames [
	"Answer a collection of the names of the class variables defined in the receiver."

	^self classPool keys sort
]

{ #category : #'class variables' }
Class >> classVariableDefinitionString [
	"Answer a string that evaluates to the definition of the class Variables"
	
	^String streamContents: [ :str | | special |
		str nextPutAll: '{ '.
		self classVariables do: [:global |
				str nextPutAll: global definitionString.
				special := global isSpecial]				
			separatedBy: [ 
				str nextPutAll: '. '.  
				special ifTrue: [ str cr;tab;tab;tab;tab ]].
		str nextPutAll: ' }'. ]
]

{ #category : #'class variables' }
Class >> classVariableNamed: aString [
	"Answer the Class Variable"

	^self classPool associationAt: aString asSymbol ifAbsent: [self error: 'no such class var']
]

{ #category : #'class variables' }
Class >> classVariables [
	"Answer the meta objects of all class variables"

	^self classPool associations
]

{ #category : #compiling }
Class >> compileAll [
	super compileAll.
	self classSide compileAll.
]

{ #category : #compiling }
Class >> compileAllFrom: oldClass [
	"Recompile all the methods in the receiver's method dictionary (not the
	subclasses). Also recompile the methods in the metaclass."

	super compileAllFrom: oldClass.
	self theMetaClass compileAllFrom: oldClass theMetaClass
]

{ #category : #copying }
Class >> copy [ 
	"Answer a copy of the receiver without a list of subclasses"
	| newClass |
	newClass := self class copy new
		superclass: superclass;
		methodDict: self methodDict copy;
		setFormat: format;
		setName: name;
		organization: self organization copy;
		classPool: classPool copy;
		sharedPools: sharedPools copy.
	self class instSize+1 to: self class instSize do:
		[:offset | newClass instVarAt: offset put: (self instVarAt: offset)].
	newClass classLayout: (layout copy host: newClass).
	^ newClass
]

{ #category : #'initialize-release' }
Class >> declareClassVariables: newVars [ 
	"Declare class variables common to all instances. Answer whether 
	recompilation is advisable."
	|  conflicts |
	
	conflicts := false.	
	"Remove. Warn if vars are removed that are still used"
	"self flag: something wrong here. put to true and add an ivar to a class"
	(self classVariables reject: [:x | newVars includes: x]) do: [:var | self removeClassVarNamed: var name interactive: false].
	
	(newVars reject: [:var | self classVariables includes: var]) do: [:var | "adding"
			"check if new vars defined elsewhere"
			(self innerBindingOf: var key) 
				ifNotNil: [(DuplicatedVariableError new) variable: var name;
						signal: var name , ' is defined elsewhere'.
					conflicts := true]].
	newVars notEmpty
		ifTrue: 
			[self classPool: self classPool.
			"in case it was nil"
			newVars do: [:var | self classPool declareVariable: var from: Undeclared]].
	^conflicts
]

{ #category : #'class variables' }
Class >> definesClassVariable: aGlobal [
	"Return whether the receiver has a class variables (shared variables among its class and subclasses) named: aString"
	
	^ self classVariables includes: aGlobal
]

{ #category : #'class variables' }
Class >> definesClassVariableNamed: aString [
	"Return whether the receiver has a class variables (shared variables among its class and subclasses) named: aString"
	
	^ self classVarNames includes: aString
]

{ #category : #accessing }
Class >> definitionForNautilus [
	"Answer a String that defines the receiver."

	<localClassMethod> 
    "This method is supposed to be local in Class because of a good reason.
    We use this pragma to test if Class does not contain some accidental 
    local selectors."

	| aStream poolString |
	((self usesSpecialSlot 
		or: [ self usesSpecialClassVariables ])
		or: [ Slot showSlotClassDefinition ])
		ifTrue: [ ^ self definitionForNautilusWithSlots ].
	poolString := self sharedPoolsString.
	aStream := (String new: 800) writeStream.
	superclass
		ifNil: [ aStream nextPutAll: 'ProtoObject' ]
		ifNotNil: [ aStream nextPutAll: superclass name ].
	aStream
		nextPutAll: self kindOfSubclass;
		store: self name.
	self hasTraitComposition
		ifTrue:
			[ aStream
				cr;
				tab;
				nextPutAll: 'uses: ';
				nextPutAll: self traitCompositionString ].
	aStream
		cr;
		tab;
		nextPutAll: 'instanceVariableNames: ';
		store: self instanceVariablesString.
	aStream
		cr;
		tab;
		nextPutAll: 'classVariableNames: ';
		store: self classVariablesString.
	poolString = ''
		ifFalse:
			[ aStream
				cr;
				tab;
				nextPutAll: 'poolDictionaries: ';
				store: poolString ].
	aStream
		cr;
		tab;
		nextPutAll: 'package: ';
		store: self category asString.
	superclass
		ifNil:
			[ aStream
				nextPutAll: '.';
				cr.
			aStream nextPutAll: self name.
			aStream
				space;
				nextPutAll: 'superclass: nil' ].
	^ aStream contents
]

{ #category : #accessing }
Class >> definitionForNautilusWithSlots [
	"the class definition with a way to specify slots. Shown when the class defines special Slot
	 NOTE: this is just for testing, the final solution will evolve and be cleaner later"

	<localClassMethod> 
    "This method is supposed to be local in Class because of a good reason.
    We use this pragma to test if Class does not contain some accidental 
    local selectors."


	| aStream poolString|

	poolString := self sharedPoolsString.

	aStream := (String new: 800) writeStream.
	superclass 
		ifNil: [aStream nextPutAll: 'ProtoObject']
		ifNotNil: [aStream nextPutAll: superclass name].
	aStream nextPutAll: ' subclass: ';
			store: self name.
	(self hasTraitComposition) ifTrue: [
		aStream cr; tab; nextPutAll: 'uses: ';
			nextPutAll: self traitCompositionString].		
			
	(self classLayout isKindOf: FixedLayout) ifFalse: [
		aStream cr; tab; nextPutAll: 'layout: ';
			nextPutAll: self classLayout class name].
	aStream cr; tab; nextPutAll: 'slots: ';
			nextPutAll: self slotDefinitionString.
	aStream cr; tab; nextPutAll: 'classVariables: ';
			nextPutAll: self classVariableDefinitionString.
	poolString = '' ifFalse: [
		aStream cr; tab; nextPutAll: 'poolDictionaries: ';
			store: poolString].
	aStream cr; tab; nextPutAll: 'category: ';
			store: self category asString.

	superclass ifNil: [ 
		aStream nextPutAll: '.'; cr.
		aStream nextPutAll: self name.
		aStream space; nextPutAll: 'superclass: nil'. ].

	^ aStream contents
]

{ #category : #copying }
Class >> duplicateClassWithNewName: aSymbol [
	| copysName class |
	copysName := aSymbol asSymbol.
	copysName = self name
		ifTrue: [ ^ self ].
	(self environment includesKey: copysName)
		ifTrue: [ ^ self error: copysName , ' already exists' ].
	class := self superclass
		subclass: copysName
		uses: self traitComposition
		instanceVariableNames: self instanceVariablesString
		classVariableNames: self classVariablesString
		poolDictionaries: self sharedPoolsString
		package: self category.
	class classSide
		instanceVariableNames: self classSide instanceVariablesString.
	class copyAllCategoriesFrom: self.
	class class copyAllCategoriesFrom: self class.
	^ class
]

{ #category : #organization }
Class >> environment [

	environment ifNil: [^ super environment].
	^ environment
]

{ #category : #organization }
Class >> environment: anEnvironment [

	environment := anEnvironment
]

{ #category : #'subclass creation - weak' }
Class >> ephemeronSubclass: className instanceVariableNames: instVarNames 
	classVariableNames: classVarNames package: cat [
	"Added to allow for a simplified subclass creation experience. "
	
	^ self ephemeronSubclass: className 
		instanceVariableNames: instVarNames 
		classVariableNames: classVarNames 
		poolDictionaries: ''
		category: cat
]

{ #category : #'subclass creation - weak' }
Class >> ephemeronSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat [
	"This is the standard initialization message for creating a new class as a subclass of an existing class (the receiver) in which the subclass is to have weak indexable pointer variables."

	^ self classInstaller make: [ :builder |
		builder
			superclass: self;
			name: t;
			layoutClass: EphemeronLayout;
			slots: f asSlotCollection;
			sharedVariablesFromString: d;
			sharedPools: s;
			category: cat;
			environment: self environment;
			copyClassSlotsFromExistingClass ].
]

{ #category : #viewer }
Class >> externalName [
	"Answer a name by which the receiver can be known."

	^ self name
]

{ #category : #private }
Class >> getName [

	<localClassMethod> 
    "This method is supposed to be local in Class because of a good reason.
    We use this pragma to test if Class does not contain some accidental 
    local selectors."

	^ name
]

{ #category : #testing }
Class >> hasAbstractMethods [
	"Tells whether the receiver locally defines an abstract method, i.e., a method sending subclassResponsibility"

	^ super hasAbstractMethods or: [ self theMetaClass hasAbstractMethods ]
]

{ #category : #'accessing parallel hierarchy' }
Class >> hasClassSide [
	^self classSide notNil.
]

{ #category : #'class variables' }
Class >> hasClassVarNamed: aString [
	"Return whether the receiver has a class variables (shared variables among its class and subclasses) named: aString"
	
	^ self classVarNames includes: aString
]

{ #category : #'class variables' }
Class >> hasClassVariable: aGlobal [
	"Return whether the receiver has a class variables (shared variables among its class and subclasses) named: aString"
	
	^ self classVariables identityIncludes: aGlobal
]

{ #category : #testing }
Class >> hasMethods [
	"Answer a Boolean according to whether any methods are defined for the 
	receiver (includes whether there are methods defined in the receiver's 
	metaclass)."

	^super hasMethods or: [ self theMetaClass hasMethods ]
]

{ #category : #'pool variables' }
Class >> hasSharedPools [
	"Returns whether the receiver uses shared pools directly (Does not take into account that it may inherit shared pool uses."
	^ self sharedPools notEmpty
]

{ #category : #'accessing class hierarchy' }
Class >> hasSubclasses [
	^subclasses notNil
]

{ #category : #'subclass creation - immediate' }
Class >> immediateSubclass: className instanceVariableNames: instvarNames 
	classVariableNames: classVarNames package: cat [
	"
	An immediate subclass define a class for which its value is coded within the OOP itself (like SmallInteger in 32 bits). It is not meant to be used by non-experimented users.
	
	Immediates are objects that are stored in an object pointer using a tag to distinguish them from ordinary object pointers.  In v3 the only immediate is SmallInteger.  In 32-but spur there are SmallInteger and Character.  An implication of this is that in spur all Characters can be compared using #==.  In 64-bit Spur there is also SmallFloat64.  If a float's exponent is in the middle 8-bits of the 11-bit exponent range then it will be immediate.  If a float's exponent is outside of the middle 8-bits it will be boxed.
	"
	^self immediateSubclass: className
		instanceVariableNames: instvarNames
		classVariableNames: classVarNames
		poolDictionaries: ''
		package: cat
]

{ #category : #'subclass creation - immediate' }
Class >> immediateSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s package: cat [
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have indexable pointer variables."
		
	^ self classInstaller make: [ :builder |
		builder
			superclass: self;
			name: t;
			layoutClass: ImmediateLayout;
			slots: f asSlotCollection;
			sharedVariablesFromString: d;
			sharedPools: s;
			category: cat;
			environment: self environment;
			copyClassSlotsFromExistingClass ].
]

{ #category : #'subclass creation - immediate' }
Class >> immediateSubclass: aClassName uses: aTraitCompositionOrArray instanceVariableNames: instVarNames 
	classVariableNames: classVarNames package: cat [
	"Added to allow for a simplified subclass creation experience. "
	
	^ self immediateSubclass: aClassName 
		uses: aTraitCompositionOrArray
		instanceVariableNames: instVarNames  
		classVariableNames: classVarNames 
		poolDictionaries: ''
		package: cat
]

{ #category : #'subclass creation - immediate' }
Class >> immediateSubclass: aName uses: aTraitComposition instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames package: aCategory [
	"Creates a new class as a subclass of an existing class (the receiver) 
	in which the subclass is to have indexable byte-sized nonpointer variables."
	
	^ self
		subclass: aName
		uses: aTraitComposition
		with: [ 
			self
				immediateSubclass: aName
				instanceVariableNames: someInstanceVariableNames
				classVariableNames: someClassVariableNames
				poolDictionaries: someSharedPoolNames
				package: aCategory ]
]

{ #category : #compiling }
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

{ #category : #testing }
Class >> isAbstract [
	"Tells whether the receiver locally defines an abstract method, i.e., a method sending subclassResponsibility"

	^ super isAbstract or: [ self theMetaClass isAbstract ]
]

{ #category : #testing }
Class >> isAnonymous [
	^self getName isNil
]

{ #category : #testing }
Class >> isClass [

	<localClassMethod> 
    "This method is supposed to be local in Class because of a good reason.
    We use this pragma to test if Class does not contain some accidental 
    local selectors."

	^ true
]

{ #category : #testing }
Class >> isClassOrTrait [
	^true
]

{ #category : #testing }
Class >> isObsolete [
	"Return true if the receiver is obsolete."

	^ self propertyAt: #obsolete ifAbsent: false
]

{ #category : #'self evaluating' }
Class >> isSelfEvaluating [
	^self isObsolete not
]

{ #category : #private }
Class >> isValidTraitName: aSymbol [

	aSymbol ifNil: [ ^ false ].
	^ aSymbol isValidGlobalName.
]

{ #category : #accessing }
Class >> name [
	"Answer the name of the receiver."

	^ name ifNil: [ super name ]
]

{ #category : #private }
Class >> name: aSymbol [
	"for compatibile API with trais"
	self setName: aSymbol
]

{ #category : #'initialize-release' }
Class >> name: aString traitComposition: aComposition methodDict: aMethodDict localSelectors: aSet organization: aClassOrganization [

	"Used by copy"
	
	self name: aString.
	self localSelectors: aSet.
	self methodDict: aMethodDict.
	self traitComposition: aComposition.
	self organization: aClassOrganization
]

{ #category : #'subclass creation' }
Class >> newAnonymousSubclass [

	<localClassMethod> 
    "This method is supposed to be local in Class because of a good reason.
    We use this pragma to test if Class does not contain some accidental 
    local selectors."

	^ Smalltalk anonymousClassInstaller make: [ :builder |
		builder
			superclass: self ;
			layoutClass: self classLayout class ].

]

{ #category : #'subclass creation' }
Class >> newSubclass [
	| i className |
	i := 1.
	[className := (self name , i printString) asSymbol.
	 self environment includesKey: className]
		whileTrue: [i := i + 1].

	^ self subclass: className
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Unclassified'

"Point newSubclass new"
]

{ #category : #'initialize-release' }
Class >> obsolete [
	"Change the receiver and all of its subclasses to an obsolete class."
	self == Object 
		ifTrue: [^self error: 'Object is NOT obsolete'].
	self setName: 'AnObsolete' , self name.
	Object class instSize + 1 to: self classSide instSize do:
		[:i | self instVarAt: i put: nil]. "Store nil over class instVars."
	self classPool: nil.
	self sharedPools: nil.
	self hasClassSide ifTrue: [ self theMetaClass obsolete].
	self propertyAt: #obsolete put: true.
	super obsolete.
]

{ #category : #compiling }
Class >> possibleVariablesFor: misspelled continuedFrom: oldResults [

	| results |
	results := misspelled correctAgainstDictionary: self classPool continuedFrom: oldResults.
	self sharedPools do: [:pool | 
		results := misspelled correctAgainstDictionary: pool classPool continuedFrom: results ].
	self superclass == nil
		ifTrue: 
			[ ^ misspelled correctAgainstDictionary: self environment continuedFrom: results ]
		ifFalse:
			[ ^ self superclass possibleVariablesFor: misspelled continuedFrom: results ]
]

{ #category : #'subclass creation' }
Class >> private_subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat [ 
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver)."
	^ self classInstaller make: [ :builder |
		builder
			superclass: self;
			name: t;
			layoutClass: self classLayout class;
			slots: f asSlotCollection;
			sharedVariablesFromString: d;
			sharedPools: s;
			category: cat;
			environment: self environment;
			copyClassSlotsFromExistingClass ].

]

{ #category : #'class variables' }
Class >> readClassVariableNamed: aString [
	"Answer the content of the Class Variable"

	^(self classVariableNamed: aString) read
]

{ #category : #compiling }
Class >> reformatAll [ 
	"Reformat all methods in this class.
	Leaves old code accessible to version browsing"

	super reformatAll.		"me..."
	self theMetaClass reformatAll.	"...and my metaclass"
]

{ #category : #'class variables' }
Class >> removeClassVarNamed: aString [
	"Remove the class variable whose name is the argument, aString, from 
    the names defined in the receiver, a class. Create an error notification if 
    aString is not a class variable or if it is still being used in the code of 
    the class."
	
	self removeClassVarNamed: aString interactive: false
]

{ #category : #'class variables' }
Class >> removeClassVarNamed: aString interactive: interactive [
	"Remove the class variable whose name is the argument, aString, from 
    the names defined in the receiver, a class. Create an error notification if 
    aString is not a class variable or if it is still being used in the code of 
    the class."

	| aSymbol varUserClass |
	aSymbol := aString asSymbol.
	(self classPool includesKey: aSymbol)
		ifFalse: [ ^ self error: aString , ' is not a class variable' ].
	varUserClass := self anyUserOfClassVarNamed: aSymbol.
	varUserClass notNil & interactive ifTrue: [ 
		(self	confirm: (aString , ' is still used in code of class ' , varUserClass name , '.\Is it okay to move it to Undeclared?') withCRs)
			ifFalse: [ ^ self ] ].
	
	varUserClass 
		ifNotNil: [self crTrace: self name , ' (' , aString , ' is Undeclared) '.
			Undeclared declare: aSymbol from: self classPool]
		ifNil: [ self classPool removeKey: aSymbol].
		
	self classPool isEmpty
		ifTrue: [ self classPool: nil ].
		
	SystemAnnouncer uniqueInstance classModificationAppliedTo: self
]

{ #category : #'class variables' }
Class >> removeClassVariable: aGlobal [
	"Remove the class variable"
	self removeClassVarNamed: aGlobal name

	
]

{ #category : #'initialize-release' }
Class >> removeFromSystem [
	"Forget the receiver from the Smalltalk global dictionary. Any existing 
	instances will refer to an obsolete version of the receiver."
	self removeFromSystem: true.
]

{ #category : #'initialize-release' }
Class >> removeFromSystem: logged [
	"Forget the receiver from the Smalltalk global dictionary. Any existing 
	instances will refer to an obsolete version of the receiver."
	
	"keep the class name and category for triggering the system change message. If we wait to long, then we get obsolete information which is not what we want."

	"tell class to deactivate and unload itself-- two separate events in the module system"
	| myCategory |
	self release.
	self unload.
	self traitUsers do: [ :user | user removeFromComposition: self ].
	self superclass ifNotNil:
		["If we have no superclass there's nothing to be remembered"
		self superclass addObsoleteSubclass: self].
	myCategory := self category.
	self environment forgetClass: self logged: logged.
	self obsolete.
	logged ifTrue: [SystemAnnouncer uniqueInstance classRemoved: self fromCategory: myCategory].
]

{ #category : #'initialize-release' }
Class >> removeFromSystemUnlogged [
	"Forget the receiver from the Smalltalk global dictionary. Any existing instances will refer to an obsolete version of the receiver.  Do not log the removal either to the current change set nor to the system changes log"
	^self removeFromSystem: false
]

{ #category : #'instance variables' }
Class >> removeInstVarNamed: aString [ 
	"Remove the argument, aString, as one of the receiver's instance variables."

	^self removeSlot: (self slotNamed: aString)
]

{ #category : #'pool variables' }
Class >> removeSharedPool: aDictionary [ 
	"Remove the pool dictionary, aDictionary, as one of the receiver's pool 
	dictionaries. Create an error notification if the dictionary is not one of 
	the pools.
	: Note that it removes the wrong one if there are two empty Dictionaries in the list."

	| satisfiedSet workingSet aSubclass |
	(self sharedPools includes: aDictionary)
		ifFalse: [^self error: 'the dictionary is not in my pool'].

	"first see if it is declared in a superclass in which case we can remove it."
	(self selectSuperclasses: [:class | class sharedPools includes: aDictionary]) isEmpty
		ifFalse: [self sharedPools remove: aDictionary.
				self sharedPools isEmpty ifTrue: [self sharedPools: nil].
				^self]. 

	"second get all the subclasses that reference aDictionary through me rather than a 
	superclass that is one of my subclasses."

	workingSet := self subclasses asOrderedCollection.
	satisfiedSet := Set new.
	[workingSet isEmpty] whileFalse:
		[aSubclass := workingSet removeFirst.
		(aSubclass sharedPools includes: aDictionary)
			ifFalse: 
				[satisfiedSet add: aSubclass.
				workingSet addAll: aSubclass subclasses]].

	"for each of these, see if they refer to any of the variables in aDictionary because 
	if they do, we can not remove the dictionary."
	satisfiedSet add: self.
	satisfiedSet do: 
		[:sub | 
		aDictionary associationsDo: 
			[:aGlobal | 
			(sub whichSelectorsReferTo: aGlobal) isEmpty 
				ifFalse: [^self error: aGlobal key 
								, ' is still used in code of class '
								, sub name]]].
	self sharedPools remove: aDictionary.
	self sharedPools isEmpty ifTrue: [self sharedPools: nil]
]

{ #category : #'accessing class hierarchy' }
Class >> removeSubclass: aSubclass [ 
	"If the argument, aSubclass, is one of the receiver's subclasses, remove it."

	self subclasses == nil ifFalse:
		[self subclasses:  (self subclasses copyWithout: aSubclass).
		self subclasses isEmpty ifTrue: [self subclasses: nil]].
]

{ #category : #'class name' }
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
from Undeclared. Check them after this change.'].
]

{ #category : #private }
Class >> setName: aSymbol [

	<localClassMethod> 
    "This method is supposed to be local in Class because of a good reason.
    We use this pragma to test if Class does not contain some accidental 
    local selectors."

	name := aSymbol.
]

{ #category : #private }
Class >> setName: aSymbol andRegisterInCategory: categorySymbol environment: aSystemDictionary [
	(self isValidTraitName: aSymbol) ifFalse: [TraitException signal: 'Invalid trait name'].
	
	(self environment == aSystemDictionary
		and: [self name = aSymbol
			and: [self category = categorySymbol]]) ifTrue: [^self].
		
	((aSystemDictionary includes: aSymbol) and: [(aSystemDictionary at: aSymbol) ~~ self])
		ifTrue: [TraitException signal: 'The name ''' , aSymbol , ''' is already used'].

	(self environment notNil and: [self name notNil and: [self name ~= aSymbol]]) ifTrue: [
		self environment renameClass: self as: aSymbol].
	
	self name: aSymbol.
	self environment: aSystemDictionary.	
	self environment at: self name put: self.
	self environment organization classify: self name under: categorySymbol.
	^ true
]

{ #category : #'pool variables' }
Class >> sharedPoolOfVarNamed: aString [
	"Returns the SharedPool or nil from which the pool variable named aString is coming from."
	
	 ^ self sharedPools 
			detect: [:each | each usesClassVarNamed: aString ]
			ifNone: [
						self superclass == nil
							ifTrue: [nil]
							ifFalse: [self superclass sharedPoolOfVarNamed: aString]
						]
]

{ #category : #'pool variables' }
Class >> sharedPools [
	"Answer an orderedCollection  of the shared pools declared in the receiver."

	^ sharedPools ifNil: [ sharedPools := OrderedCollection new ]
]

{ #category : #'pool variables' }
Class >> sharedPools: aCollection [
	sharedPools := aCollection
]

{ #category : #'initialize-release' }
Class >> sharing: poolString [ 
	"Set up sharedPools. Answer whether recompilation is advisable."
	| oldPools |
	oldPools := self sharedPools.
	self sharedPools: OrderedCollection new.
	(poolString substrings: ' ') do: 
		[:poolName | 
		self sharedPools add: (self environment at: poolName asSymbol ifAbsent:[
			(self confirm: 'The pool dictionary ', poolName,' does not exist.',
						'\Do you want it automatically created?' withCRs)
				ifTrue: [ self classInstaller make: [ :builder | 
							builder superclass: SharedPool;
							name: poolName;
							category: self category ]]
				ifFalse:[^self error: poolName,' does not exist']])].
	self sharedPools isEmpty ifTrue: [self sharedPools: nil].
	oldPools do: [:pool |
				| found |
				found := self sharedPools anySatisfy: [:p | p == pool].
				found ifFalse: [^ true "A pool got deleted"]].
	^ false
]

{ #category : #'subclass creation' }
Class >> subclass: t [
	^ self subclass: t instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Unclassified' 
]

{ #category : #'subclass creation' }
Class >> subclass: t instanceVariableNames: ins [
	^ self subclass: t instanceVariableNames: ins classVariableNames: '' poolDictionaries: '' category: 'Unclassified' 
]

{ #category : #'subclass creation' }
Class >> subclass: aSubclassSymbol instanceVariableNames: instVarNames classVariableNames: classVarNames category: aCategorySymbol [
	"Added to allow for a simplified subclass creation experience. "
	^ self
		subclass: aSubclassSymbol
		instanceVariableNames: instVarNames
		classVariableNames: classVarNames
		poolDictionaries: ''
		category: aCategorySymbol
]

{ #category : #'subclass creation' }
Class >> subclass: aSubclassSymbol instanceVariableNames: instVarNames classVariableNames: classVarNames package: aPackageSymbol [
	"Added to allow for a simplified subclass creation experience. "

	^ self
		subclass: aSubclassSymbol
		instanceVariableNames: instVarNames
		classVariableNames: classVarNames
		poolDictionaries: ''
		package: aPackageSymbol
]

{ #category : #'subclass creation' }
Class >> subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat [
	^ self
		subclass: t
		uses: {}
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat
]

{ #category : #'subclass creation' }
Class >> subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s package: cat [
	^ self
		subclass: t
		uses: {}
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		package: cat
]

{ #category : #'subclass creation - slots' }
Class >> subclass: aSubclassSymbol  layout: layoutClass slots: slotDefinition classVariables: classVarDefinition category: aCategorySymbol [

	^ self classInstaller make: [ :builder |
		builder name: aSubclassSymbol;
		superclass: self;
		layoutClass: layoutClass;
		slots: slotDefinition;
		sharedVariables: classVarDefinition;
		category: aCategorySymbol ].

]

{ #category : #'subclass creation - slots' }
Class >> subclass: aSubclassSymbol  layout: layoutClass  slots: slotDefinition classVariables: classVarDefinition poolDictionaries: someSharedPoolNames category: aCategorySymbol [

	^ self classInstaller make: [ :builder |
		builder name: aSubclassSymbol;
		superclass: self;
		layoutClass: layoutClass;
		slots: slotDefinition;
		sharedVariables:  classVarDefinition;
		sharedPools: someSharedPoolNames;
		category: aCategorySymbol ].

]

{ #category : #'subclass creation - slots' }
Class >> subclass: aSubclassSymbol slots: slotDefinition classVariables: classVarDefinition category: aCategorySymbol [

	^ self classInstaller make: [ :builder | 
			builder
				name: aSubclassSymbol;
				superclass: self;
				slots: slotDefinition;
				sharedVariables: classVarDefinition;
				category: aCategorySymbol ]
]

{ #category : #'subclass creation - slots' }
Class >> subclass: aSubclassSymbol slots: slotDefinition classVariables: classVarDefinition poolDictionaries: someSharedPoolNames category: aCategorySymbol [

	^ self classInstaller make: [ :builder |
		builder name: aSubclassSymbol;
		superclass: self;
		slots: slotDefinition;
		sharedVariables:  classVarDefinition;
		sharedPools: someSharedPoolNames;
		category: aCategorySymbol ].

]

{ #category : #'subclass creation' }
Class >> subclass: t uses: aTraitComposition [ 
	| cls |
	cls := self
				subclass: t
				instanceVariableNames: ''
				classVariableNames: ''
				poolDictionaries: ''
				category: 'Unclassified'.
	cls setTraitComposition: aTraitComposition asTraitComposition.
	^ cls
]

{ #category : #'subclass creation' }
Class >> subclass: aTraitName uses: aTraitCompositionOrArray instanceVariableNames: instVarNames classVariableNames: classVarNames category: cat [ 

	^ self
		subclass: aTraitName
		uses: aTraitCompositionOrArray
		instanceVariableNames: instVarNames
		classVariableNames: classVarNames
		poolDictionaries: ''
		category: cat 
]

{ #category : #'subclass creation' }
Class >> subclass: aTraitName uses: aTraitCompositionOrArray instanceVariableNames: instVarNames classVariableNames: classVarNames package: cat [
	^ self
		subclass: aTraitName
		uses: aTraitCompositionOrArray
		instanceVariableNames: instVarNames
		classVariableNames: classVarNames
		category: cat
]

{ #category : #'subclass creation' }
Class >> subclass: aName uses: aTraitCompositionOrArray instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames category: aCategory [

	^ self
		subclass: aName
		uses: aTraitCompositionOrArray
		with: [ 
			self
				private_subclass: aName
				instanceVariableNames: someInstanceVariableNames
				classVariableNames: someClassVariableNames
				poolDictionaries: someSharedPoolNames
				category: aCategory ]
]

{ #category : #'subclass creation' }
Class >> subclass: aName uses: aTraitCompositionOrArray instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames package: aCategory [

	^ self
		subclass: aName
		uses: aTraitCompositionOrArray
		with: [ 
			self
				private_subclass: aName
				instanceVariableNames: someInstanceVariableNames
				classVariableNames: someClassVariableNames
				poolDictionaries: someSharedPoolNames
				category: aCategory ]
]

{ #category : #'subclass creation - slots' }
Class >> subclass: aSubclassSymbol uses: aTraitCompositionOrArray  layout: layoutClass slots: slotDefinition classVariables: classVarDefinition category: aCategorySymbol [
	^ self
		subclass: aSubclassSymbol
		uses: aTraitCompositionOrArray
		with: [ 
			self classInstaller
				make: [ :builder | 
					builder
						name: aSubclassSymbol;
						superclass: self;
						layoutClass: layoutClass;
						slots: slotDefinition;
						sharedVariables: classVarDefinition;
						category: aCategorySymbol ] ]
]

{ #category : #'subclass creation - slots' }
Class >> subclass: aSubclassSymbol uses: aTraitCompositionOrArray  layout: layoutClass slots: slotDefinition classVariables: classVarDefinition poolDictionaries: someSharedPoolNames category: aCategorySymbol [
	^ self
		subclass: aSubclassSymbol
		uses: aTraitCompositionOrArray
		with: [ 
			self classInstaller
				make: [ :builder | 
					builder
						name: aSubclassSymbol;
						superclass: self;
						layoutClass: layoutClass;
						slots: slotDefinition;
						sharedVariables: classVarDefinition;
						sharedPools: someSharedPoolNames;
						category: aCategorySymbol ] ]
]

{ #category : #'subclass creation - slots' }
Class >> subclass: aSubclassSymbol uses: aTraitCompositionOrArray slots: slotDefinition classVariables: classVarDefinition category: aCategorySymbol [
	^ self
		subclass: aSubclassSymbol
		uses: aTraitCompositionOrArray
		with: [ 
			self classInstaller
				make: [ :builder | 
					builder
						name: aSubclassSymbol;
						superclass: self;
						slots: slotDefinition;
						sharedVariables: classVarDefinition;
						category: aCategorySymbol ] ]
]

{ #category : #'subclass creation - slots' }
Class >> subclass: aSubclassSymbol uses: aTraitCompositionOrArray slots: slotDefinition classVariables: classVarDefinition poolDictionaries: someSharedPoolNames category: aCategorySymbol [
	^ self
		subclass: aSubclassSymbol
		uses: aTraitCompositionOrArray
		with: [ 
			self classInstaller
				make: [ :builder | 
					builder
						name: aSubclassSymbol;
						superclass: self;
						slots: slotDefinition;
						sharedVariables: classVarDefinition;
						sharedPools: someSharedPoolNames;
						category: aCategorySymbol ] ]
]

{ #category : #'subclass creation - private' }
Class >> subclass: aName uses: aTraitComposition with: aBlock [ 
	"Define (or redefine) a subclass by evaluating a block closure. 

	Note (MartinDias): 
		This is a workaround for the subclass creation API when a trait is specified. 
		To fix it well, in Pharo 4, a bit deeper refactoring is required, delegating this responsibility
		to SlotClassBuilder."

	^ self environment 
		at: aName
		ifPresent: [ :old | 
			| copyOfOld newClass |
			copyOfOld := old copy.

			"Modify the class and set the new trait composition"
			newClass := aBlock value. 
			newClass setTraitComposition: aTraitComposition asTraitComposition.

			"Announce that the definition changed when there is a new trait composution. 
			Note: It may be a re-announce if there was other change in the class definition, 
			e.g. an instace variable added."
			(newClass traitComposition syntacticallyEquals: copyOfOld traitComposition) ifFalse: [ 
				SystemAnnouncer uniqueInstance 
					classDefinitionChangedFrom: copyOfOld to: newClass;
					classModificationAppliedTo: newClass ].

			newClass ]
		ifAbsent: [ | newClass |
			"Create the class and set the trait composition"
			newClass := aBlock value.
			newClass setTraitComposition: aTraitComposition asTraitComposition.
			SystemAnnouncer uniqueInstance classModificationAppliedTo: newClass.
			newClass].
]

{ #category : #'accessing class hierarchy' }
Class >> subclasses [
	"Answer a Set containing the receiver's subclasses."

	^subclasses 
		ifNil: [ #() ]
		ifNotNil: [ subclasses copy ]
]

{ #category : #'accessing class hierarchy' }
Class >> subclasses: aCollection [
	subclasses := aCollection.
]

{ #category : #'accessing class hierarchy' }
Class >> subclassesDo: aBlock [ 
	"Evaluate the argument, aBlock, for each of the receiver's immediate subclasses."
	
	self subclasses do: aBlock
]

{ #category : #'initialize-release' }
Class >> superclass: aClass methodDictionary: mDict format: fmt [
	"Basic initialization of the receiver"
	super superclass: aClass methodDictionary: mDict format: fmt.
	self subclasses: nil. 
]

{ #category : #accessing }
Class >> traitComposition [

	<localClassMethod> 
    "This method is supposed to be local in Class because of a good reason.
    We use this pragma to test if Class does not contain some accidental 
    local selectors."

	traitComposition ifNil: [traitComposition := TraitComposition new].
	^traitComposition
]

{ #category : #accessing }
Class >> traitComposition: aTraitComposition [

	<localClassMethod> 
    "This method is supposed to be local in Class because of a good reason.
    We use this pragma to test if Class does not contain some accidental 
    local selectors."
	
	traitComposition := aTraitComposition
]

{ #category : #'initialize-release' }
Class >> unload [
	"Sent when a the class is removed.  Does nothing, but may be overridden by (class-side) subclasses."
]

{ #category : #'class variables' }
Class >> usesClassVarNamed: aString [
	"Return whether the receiver or its superclasses have a class variable named: aString"
	
	^ self allClassVarNames includes: aString
]

{ #category : #'pool variables' }
Class >> usesLocalPoolVarNamed: aString [
	"Return whether the receiver uses a pool variable named: aString which is defined locally"
	
	self hasSharedPools
		ifTrue: [ self sharedPools do: [:each | (each usesClassVarNamed: aString) 
												ifTrue: [ ^true ]]]
		ifFalse: [ ^false ].
	^false
]

{ #category : #'pool variables' }
Class >> usesPoolVarNamed: aString [
	"Return whether the receiver has a pool variable named: aString, taking into account superclasses too"
	
	self allSharedPools do: [:each | (each usesClassVarNamed: aString) ifTrue: [^true]].
	^false
]

{ #category : #'subclass creation - variableByte' }
Class >> variableByteSubclass: className instanceVariableNames: instvarNames 
	classVariableNames: classVarNames category: cat [
	"Added to allow for a simplified subclass creation experience. "
	^self variableByteSubclass: className
		instanceVariableNames: instvarNames
		classVariableNames: classVarNames
		poolDictionaries: ''
		category: cat
]

{ #category : #'subclass creation - variableByte' }
Class >> variableByteSubclass: className instanceVariableNames: instvarNames classVariableNames: classVarNames package: cat [
	"Added to allow for a simplified subclass creation experience. "

	^ self
		variableByteSubclass: className
		instanceVariableNames: instvarNames
		classVariableNames: classVarNames
		category: cat
]

{ #category : #'subclass creation - variableByte' }
Class >> variableByteSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat [
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have indexable byte-sized nonpointer variables."
	
	| oldClassOrNil actualLayoutClass |
	oldClassOrNil := self environment at: t ifAbsent:[nil].
	actualLayoutClass := (oldClassOrNil notNil
				   and: [oldClassOrNil classLayout class  == CompiledMethodLayout ])
					ifTrue: [CompiledMethodLayout]
					ifFalse: [ByteLayout].
					
	^ self classInstaller make: [ :builder |
		builder
			superclass: self;
			name: t;
			layoutClass: actualLayoutClass;
			slots: f asSlotCollection;
			sharedVariablesFromString: d;
			sharedPools: s;
			category: cat;
			environment: self  environment;
			copyClassSlotsFromExistingClass ].
]

{ #category : #'subclass creation - variableByte' }
Class >> variableByteSubclass: className uses: aTraitCompositionOrArray instanceVariableNames: instVarNames 
	classVariableNames: classVarNames category: cat [
		"Added to allow for a simplified subclass creation experience. "
	
	^ self variableByteSubclass: className
		uses: aTraitCompositionOrArray
		instanceVariableNames: instVarNames
		classVariableNames: classVarNames 
		poolDictionaries: ''
		category: cat
]

{ #category : #'subclass creation - variableByte' }
Class >> variableByteSubclass: aName uses: aTraitComposition instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames category: aCategory [
	"Creates a new class as a subclass of an existing class (the receiver) 
	in which the subclass is to have indexable byte-sized nonpointer variables."
	
	^ self
		subclass: aName
		uses: aTraitComposition
		with: [ 
			self
				variableByteSubclass: aName
				instanceVariableNames: someInstanceVariableNames
				classVariableNames: someClassVariableNames
				poolDictionaries: someSharedPoolNames
				category: aCategory ]
]

{ #category : #'subclass creation - variable' }
Class >> variableSubclass: className instanceVariableNames: instVarNames classVariableNames: classVarNames category: cat [
	"Added to allow for a simplified subclass creation experience. "

	^ self variableSubclass: className
		instanceVariableNames: instVarNames 
		classVariableNames: classVarNames 
		poolDictionaries: ''
		category: cat

]

{ #category : #'subclass creation - variable' }
Class >> variableSubclass: className instanceVariableNames: instVarNames classVariableNames: classVarNames package: cat [
	^self variableSubclass: className instanceVariableNames: instVarNames classVariableNames: classVarNames category: cat
]

{ #category : #'subclass creation - variable' }
Class >> variableSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat [
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have indexable pointer variables."
		
	^ self classInstaller make: [ :builder |
		builder
			superclass: self;
			name: t;
			layoutClass: VariableLayout;
			slots: f asSlotCollection;
			sharedVariablesFromString: d;
			sharedPools: s;
			category: cat;
			environment: self environment;
			copyClassSlotsFromExistingClass ].
]

{ #category : #'subclass creation - variable' }
Class >> variableSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s package: cat [
	^ self variableSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat
]

{ #category : #'subclass creation - variable' }
Class >> variableSubclass: aClassName uses: aTraitCompositionOrArray instanceVariableNames: instVarNames 
	classVariableNames: classVarNames category: cat [
	"Added to allow for a simplified subclass creation experience. "
	
	^ self variableSubclass: aClassName 
		uses: aTraitCompositionOrArray
		instanceVariableNames: instVarNames  
		classVariableNames: classVarNames 
		poolDictionaries: ''
		category: cat
]

{ #category : #'subclass creation - variable' }
Class >> variableSubclass: aClassName uses: aTraitCompositionOrArray instanceVariableNames: instVarNames classVariableNames: classVarNames package: cat [

	^ self variableSubclass: aClassName uses: aTraitCompositionOrArray instanceVariableNames: instVarNames classVariableNames: classVarNames category: cat

]

{ #category : #'subclass creation - variable' }
Class >> variableSubclass: aName uses: aTraitComposition instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames category: aCategory [
	"Creates a new class as a subclass of an existing class (the receiver) 
	in which the subclass is to have indexable pointer variables."
	
	^ self
		subclass: aName
		uses: aTraitComposition
		with: [ 
			self
				variableSubclass: aName
				instanceVariableNames: someInstanceVariableNames
				classVariableNames: someClassVariableNames
				poolDictionaries: someSharedPoolNames
				category: aCategory ]
]

{ #category : #'subclass creation - variable' }
Class >> variableSubclass: aName uses: aTraitComposition instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames package: aCategory [
	^ self variableSubclass: aName uses: aTraitComposition instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames category: aCategory
]

{ #category : #'subclass creation - variableWord' }
Class >> variableWordSubclass: className instanceVariableNames: instVarNames
	classVariableNames: classVarNames category: cat [
	"Added to allow for a simplified subclass creation experience. "
	 
	^ self variableWordSubclass: className
		instanceVariableNames: instVarNames  
		classVariableNames: classVarNames 
		poolDictionaries: ''
		package: cat
]

{ #category : #'subclass creation - variableWord' }
Class >> variableWordSubclass: className instanceVariableNames: instVarNames
	classVariableNames: classVarNames package: cat [
	"
	Variable word is like variable byte (ByteArray), variable size, with indices instead of named instance variables, but each index points to a full word (either 32 or 64bit depending on the VM).
	
	Objects on the heap are either pointers or bits.  For example instances of Point, Array, BlockClosure etc are pointer objects.  But Bitmap, ByteString, WideString etc are bits objects.  In v3 bits objects are either a sequence of bytes (ByteArray, ByteString, ByteSymbol etc) or 32-bit words (WideString, Float, Bitmap etc).  Spur supports byte, short, word and double-word bits objects even though currently only byte and word classes exist.  16-bit strings will be useful on Windows, for example.
	"
	 
	^ self variableWordSubclass: className
		instanceVariableNames: instVarNames  
		classVariableNames: classVarNames 
		poolDictionaries: ''
		package: cat
]

{ #category : #'subclass creation - variableWord' }
Class >> variableWordSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat [
	^ self
		variableWordSubclass: t
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		package: cat
]

{ #category : #'subclass creation - variableWord' }
Class >> variableWordSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s package: cat [
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have indexable word-sized nonpointer variables."
	^ self classInstaller make: [ :builder |
		builder
			superclass: self;
			name: t;
			layoutClass: WordLayout;
			slots: f asSlotCollection;
			sharedVariablesFromString: d;
			sharedPools: s;
			category: cat;
			environment: self environment;
			copyClassSlotsFromExistingClass ].
]

{ #category : #'subclass creation - variableWord' }
Class >> variableWordSubclass: className uses: aTraitCompositionOrArray instanceVariableNames: instVarNames classVariableNames: classVarNames category: cat [
	"Added to allow for a simplified subclass creation experience. "

	^ self
		variableWordSubclass: className
		uses: aTraitCompositionOrArray
		instanceVariableNames: instVarNames
		classVariableNames: classVarNames
		package: cat
]

{ #category : #'subclass creation - variableWord' }
Class >> variableWordSubclass: className uses: aTraitCompositionOrArray instanceVariableNames: instVarNames
	classVariableNames: classVarNames package: cat [
	"Added to allow for a simplified subclass creation experience. "

	^ self variableWordSubclass: className
		uses: aTraitCompositionOrArray
		instanceVariableNames: instVarNames
		classVariableNames: classVarNames
		poolDictionaries: ''
		package: cat
]

{ #category : #'subclass creation - variableWord' }
Class >> variableWordSubclass: aName uses: aTraitComposition instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames category: aCategory [
	^ self
		variableWordSubclass: aName
		uses: aTraitComposition
		instanceVariableNames: someInstanceVariableNames
		classVariableNames: someClassVariableNames
		poolDictionaries: someSharedPoolNames
		package: aCategory
]

{ #category : #'subclass creation - variableWord' }
Class >> variableWordSubclass: aName uses: aTraitComposition instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames package: aCategory [
	"Creates a new class as a subclass of an existing class (the receiver) 
	in which the subclass is to have indexable word-sized nonpointer variables."
	
	^ self
		subclass: aName
		uses: aTraitComposition
		with: [ 
			self
				variableWordSubclass: aName
				instanceVariableNames: someInstanceVariableNames
				classVariableNames: someClassVariableNames
				poolDictionaries: someSharedPoolNames
				package: aCategory ]
]

{ #category : #'subclass creation - weak' }
Class >> weakSubclass: className instanceVariableNames: instVarNames 
	classVariableNames: classVarNames category: cat [
	"Added to allow for a simplified subclass creation experience. "
	
	^ self weakSubclass: className 
		instanceVariableNames: instVarNames 
		classVariableNames: classVarNames 
		poolDictionaries: ''
		category: cat
]

{ #category : #'subclass creation - weak' }
Class >> weakSubclass: className instanceVariableNames: instVarNames 
	classVariableNames: classVarNames package: cat [
	"Added to allow for a simplified subclass creation experience. "
	
	^ self weakSubclass: className 
		instanceVariableNames: instVarNames 
		classVariableNames: classVarNames 
		poolDictionaries: ''
		category: cat
]

{ #category : #'subclass creation - weak' }
Class >> weakSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat [
	"This is the standard initialization message for creating a new class as a subclass of an existing class (the receiver) in which the subclass is to have weak indexable pointer variables."

	^ self classInstaller make: [ :builder |
		builder
			superclass: self;
			name: t;
			layoutClass: WeakLayout;
			slots: f asSlotCollection;
			sharedVariablesFromString: d;
			sharedPools: s;
			category: cat;
			environment: self environment;
			copyClassSlotsFromExistingClass ].
]

{ #category : #'subclass creation - weak' }
Class >> weakSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s package: cat [
	"This is the standard initialization message for creating a new class as a subclass of an existing class (the receiver) in which the subclass is to have weak indexable pointer variables."

	^self weakSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat
]

{ #category : #'subclass creation - weak' }
Class >> weakSubclass: className uses: aTraitCompositionOrArray instanceVariableNames: instVarNames 
	classVariableNames: classVarNames category: cat [
	"Added to allow for a simplified subclass creation experience. "
	
	^ self weakSubclass: className
		uses: aTraitCompositionOrArray
		instanceVariableNames: instVarNames 
		classVariableNames: classVarNames 
		poolDictionaries: ''
		category: cat

]

{ #category : #'subclass creation - weak' }
Class >> weakSubclass: className uses: aTraitCompositionOrArray instanceVariableNames: instVarNames 
	classVariableNames: classVarNames package: cat [
	"Added to allow for a simplified subclass creation experience. "
	
	^self weakSubclass: className uses: aTraitCompositionOrArray instanceVariableNames: instVarNames classVariableNames: classVarNames category: cat
]

{ #category : #'subclass creation - weak' }
Class >> weakSubclass: aName uses: aTraitComposition instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames category: aCategory [
	"Creates a new class as a subclass of an existing class (the receiver) 
	in which the subclass is to have weak indexable pointer variables."
	
	^ self
		subclass: aName
		uses: aTraitComposition
		with: [ 
			self
				weakSubclass: aName
				instanceVariableNames: someInstanceVariableNames
				classVariableNames: someClassVariableNames
				poolDictionaries: someSharedPoolNames
				category: aCategory ]
]

{ #category : #'subclass creation - weak' }
Class >> weakSubclass: aName uses: aTraitComposition instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames package: aCategory [
	"Creates a new class as a subclass of an existing class (the receiver) 
	in which the subclass is to have weak indexable pointer variables."
	
	^self weakSubclass: aName uses: aTraitComposition instanceVariableNames: someInstanceVariableNames classVariableNames: someClassVariableNames poolDictionaries: someSharedPoolNames category: aCategory
]

{ #category : #'class variables' }
Class >> writeClassVariableNamed: aString value: anObject [
	"Store anObject in the class variable."
	
	(self classVariableNamed: aString) write: anObject
]
