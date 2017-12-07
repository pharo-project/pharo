"
My instances add instance-specific behavior to various class-describing objects in the system. This typically includes messages for initializing class variables and instance creation messages particular to a class. There is only one instance of a particular Metaclass, namely the class which is being described. A Metaclass shares the class variables of its instance.
	
[Subtle] In general, the superclass hierarchy for metaclasses parallels that for classes. Thus,
	Integer superclass == Number, and
	Integer class superclass == Number class.
However there is a singularity at Object. Here the class hierarchy terminates, but the metaclass hierarchy must wrap around to Class, since ALL metaclasses are subclasses of Class. Thus,
	Object superclass == nil, and
	Object class superclass == Class.

[Subtle detail] A class is know by name to an environment.  Typically this is the SystemDictionary named Smalltalk.  If we ever make lightweight classes that are not in Smalltalk, they must be in some environment.  Specifically, the code that sets 'wasPresent' in name:inEnvironment:subclassOf:instanceVariableNames:variable:words:pointers:classVariableNames:poolDictionaries:category:comment:changed: must continue to work.
"
Class {
	#name : #Metaclass,
	#superclass : #ClassDescription,
	#instVars : [
		'thisClass',
		'traitComposition',
		'localSelectors'
	],
	#category : #'Kernel-Classes'
}

{ #category : #compiling }
Metaclass >> acceptsLoggingOfCompilation [
	"Answer whether the receiver's method submisions and class defintions should be logged to the changes file and to the current change set.  The metaclass follows the rule of the class itself."

	^ self theNonMetaClass acceptsLoggingOfCompilation
]

{ #category : #'instance variables' }
Metaclass >> addInstVarNamed: aString [ 
	"Add the argument, aString, as one of the receiver's instance variables."

	| fullString |
	fullString := String streamContents: [:strm |
		self instVarNames do: [:aString2 | strm nextPutAll: aString2; space].
		strm nextPutAll: aString].
	self instanceVariableNames: fullString
]

{ #category : #'class hierarchy' }
Metaclass >> addObsoleteSubclass: aClass [
	"Do nothing."
]

{ #category : #'instance variables' }
Metaclass >> addSlot: aClassSlot [

	<localMetaclassMethod> 
    "This method is supposed to be local in Metaclass because of a good reason.
    We use this pragma to test if Metaclass does not contain some accidental 
    local selectors."

	^self theNonMetaClass addClassSlot: aClassSlot
]

{ #category : #'class hierarchy' }
Metaclass >> addSubclass: aClass [
	"Do nothing."
]

{ #category : #'initialize-release' }
Metaclass >> adoptInstance: oldInstance from: oldMetaClass [ 
	"Recreate any existing instances of the argument, oldClass, as instances of 
	the receiver, which is a newly changed class. Permute variables as 
	necessary."
	thisClass class == self ifTrue:[^self error:'Metaclasses have only one instance'].
	oldMetaClass isMeta ifFalse:[^self error:'Argument must be Metaclass'].
	oldInstance class == oldMetaClass ifFalse:[^self error:'Not the class of argument'].
	^thisClass := self 
		newInstanceFrom: oldInstance 
		variable: self isVariable 
		size: self instSize 
		map: (self instVarMappingFrom: oldMetaClass)
]

{ #category : #composition }
Metaclass >> assertConsistantCompositionsForNew: aTraitComposition [
	"Applying or modifying a trait composition on the class side
	of a behavior has some restrictions."

	| baseTraits notAddable message |
	baseTraits := aTraitComposition traits select: [:each | each isBaseTrait].
	baseTraits isEmpty ifFalse: [
		notAddable := (baseTraits reject: [:each | each classSide methodDict isEmpty]).
		notAddable isEmpty ifFalse: [
			message := String streamContents: [:stream |
				stream nextPutAll: 'You can not add the base trait(s)'; cr.
				notAddable
					do: [:each | stream nextPutAll: each name]
					separatedBy: [ stream nextPutAll: ', '].
				stream cr; nextPutAll: 'to this composition because it/they define(s) methods on the class side.'].
		^TraitCompositionException signal: message]].
		
	(self instanceSide traitComposition traits asSet =
			(aTraitComposition traits
				select: [:each | each isClassTrait]
				thenCollect: [:each | each baseTrait]) asSet) ifFalse: [
				^TraitCompositionException signal: 'You can not add or remove class side traits on
				the class side of a composition. (But you can specify aliases or exclusions
				for existing traits or add a trait which does not have any methods on the class side.)']
]

{ #category : #'accessing parallel hierarchy' }
Metaclass >> baseClass [

	<localMetaclassMethod> 
    "This method is supposed to be local in Metaclass because of a good reason.
    We use this pragma to test if Metaclass does not contain some accidental 
    local selectors."

	^thisClass.
]

{ #category : #accessing }
Metaclass >> basicLocalSelectors [
	"Direct accessor for the instance variable localSelectors.
	Since localSelectors is lazily initialized, this may 
	return nil, which means that all selectors are local."

	<localMetaclassMethod> 
    "This method is supposed to be local in Metaclass because of a good reason.
    We use this pragma to test if Metaclass does not contain some accidental 
    local selectors."

	^ localSelectors
]

{ #category : #accessing }
Metaclass >> basicLocalSelectors: aSetOrNil [

	<localMetaclassMethod> 
    "This method is supposed to be local in Metaclass because of a good reason.
    We use this pragma to test if Metaclass does not contain some accidental 
    local selectors."

	localSelectors := aSetOrNil
]

{ #category : #compiling }
Metaclass >> binding [
	"return an association that can be used as the binding
	 To share it between methods, reuse an existing one if possible"
	^self methodDict 
		ifEmpty: [nil -> self]
		ifNotEmpty: [:dict | dict anyOne classBinding]
]

{ #category : #compiling }
Metaclass >> bindingOf: varName [

	^self theNonMetaClass classBindingOf: varName
]

{ #category : #testing }
Metaclass >> canZapMethodDictionary [
	"Return true if it is safe to zap the method dictionary on #obsolete"
	self soleInstance == nil
		ifTrue:[^true]
		ifFalse:[^self soleInstance canZapMethodDictionary]
]

{ #category : #accessing }
Metaclass >> category [
	^ self theNonMetaClass category
]

{ #category : #'accessing parallel hierarchy' }
Metaclass >> classClass [

	<localMetaclassMethod> 
    "This method is supposed to be local in Metaclass because of a good reason.
    We use this pragma to test if Metaclass does not contain some accidental 
    local selectors."

	^self
]

{ #category : #'pool variables' }
Metaclass >> classPool [
	"Answer the dictionary of class variables."

	^self theNonMetaClass classPool
]

{ #category : #'accessing instances and variables' }
Metaclass >> classVarNames [
	"Answer a set of the names of the class variables defined in the receiver's instance."
	
	self theNonMetaClass ifNil: [ ^ Set new ].
	^self theNonMetaClass classVarNames
]

{ #category : #'fileIn/Out' }
Metaclass >> definition [
	"Refer to the comment in ClassDescription|definition."

	^ String streamContents: 
		[:strm |
		strm print: self.
		self hasTraitComposition ifTrue: [
			strm
				crtab;
				nextPutAll: 'uses: ';
				print: self traitComposition ].
		
		(self usesSpecialSlot or: [ Slot showSlotClassDefinition ])
			ifFalse: [  
						strm
							crtab;
							nextPutAll: 'instanceVariableNames: ';
							store: self instanceVariablesString]
			ifTrue: [ 
						strm
							crtab;
							nextPutAll: 'slots: ';
							nextPutAll: self slotDefinitionString]]
]

{ #category : #accessing }
Metaclass >> environment [

	<localMetaclassMethod> 
    "This method is supposed to be local in Metaclass because of a good reason.
    We use this pragma to test if Metaclass does not contain some accidental 
    local selectors."

	^thisClass environment
]

{ #category : #'accessing hierarchy protocol' }
Metaclass >> hasClassSide [
	^false
]

{ #category : #initialize }
Metaclass >> initializeFrom: anotherClassTrait [
	self traitComposition: self traitComposition copyTraitExpression.
	self methodDict: self methodDict copy.
	self localSelectors: self localSelectors copy.
	self basicOrganization: self organization copy.
]

{ #category : #testing }
Metaclass >> isAnonymous [
	^self soleInstance isAnonymous 
]

{ #category : #testing }
Metaclass >> isClass [

	<localMetaclassMethod> 
    "This method is supposed to be local in Metaclass because of a good reason.
    We use this pragma to test if Metaclass does not contain some accidental 
    local selectors."

	^ true
]

{ #category : #testing }
Metaclass >> isMeta [
	^ true
]

{ #category : #'class hierarchy' }
Metaclass >> isMetaclassOfClassOrNil [
	
	
	^ self theNonMetaClass 
		ifNil: [ true ] 
		ifNotNil: [ :nonMetaClass | nonMetaClass == Class ] 
]

{ #category : #testing }
Metaclass >> isObsolete [
	"Return true if the receiver is obsolete"
	^self soleInstance == nil "Either no thisClass"
		or:[self soleInstance classSide ~~ self "or I am not the class of thisClass"
			or:[self soleInstance isObsolete]] "or my instance is obsolete"
]

{ #category : #testing }
Metaclass >> isSelfEvaluating [
	^self isObsolete not
]

{ #category : #testing }
Metaclass >> isUsed [
	"Metaclasses are used by default"
	^ true
]

{ #category : #accessing }
Metaclass >> name [
	"Answer a String that is the name of the receiver, either 'Metaclass' or 
	the name of the receiver's class followed by ' class'."

	^ thisClass isNil
		ifTrue: [ 'a Metaclass' ]
		ifFalse: [ thisClass name asString, ' class' ]
]

{ #category : #'instance creation' }
Metaclass >> new [
	"The receiver can only have one instance. Create it or complain that
	one already exists."

	thisClass class ~~ self
		ifTrue: [^thisClass := self basicNew]
		ifFalse: [self error: 'A Metaclass should only have one instance!']
]

{ #category : #'instance creation' }
Metaclass >> newAnonymousSubclass [

	<localMetaclassMethod> 
    "This method is supposed to be local in Metaclass because of a good reason.
    We use this pragma to test if Metaclass does not contain some accidental 
    local selectors."

	^self theNonMetaClass newAnonymousSubclass class
]

{ #category : #composition }
Metaclass >> noteNewBaseTraitCompositionApplied: aTraitComposition [
	"The argument is the new trait composition of my base trait - add
	the new traits or remove non existing traits on my class side composition.
	(Each class trait in my composition has its base trait on the instance side
	of the composition - manually added traits to the class side are always 
	base traits.)"
	
	| newComposition traitsFromInstanceSide |
	traitsFromInstanceSide := self traitComposition traits
		select: [:each | each isClassTrait]
		thenCollect: [:each | each baseTrait].
		
	newComposition := self traitComposition copyTraitExpression.
	(traitsFromInstanceSide copyWithoutAll: aTraitComposition traits) do: [:each |
		newComposition removeFromComposition: each classTrait].
	(aTraitComposition traits copyWithoutAll: traitsFromInstanceSide) do: [:each |
		newComposition add:  (each classTrait)].

	self setTraitComposition: newComposition
]

{ #category : #'class hierarchy' }
Metaclass >> obsoleteSubclasses [
	"Answer the receiver's subclasses."

	self isMetaclassOfClassOrNil ifTrue: [ ^ #() ].

	^ self theNonMetaClass obsoleteSubclasses collect: [ :aSubclass | aSubclass theMetaClass ]
]

{ #category : #compiling }
Metaclass >> possibleVariablesFor: misspelled continuedFrom: oldResults [

	^ self theNonMetaClass possibleVariablesFor: misspelled continuedFrom: oldResults

]

{ #category : #copying }
Metaclass >> postCopy [
	"Don't share the reference to the sole instance."

	<localMetaclassMethod> 
    "This method is supposed to be local in Metaclass because of a good reason.
    We use this pragma to test if Metaclass does not contain some accidental 
    local selectors."

	super postCopy.
	thisClass := nil.
]

{ #category : #'instance variables' }
Metaclass >> removeInstVarNamed: aString [ 
	"Remove the argument, aString, as one of the receiver's instance variables."

	| newArray newString |
	(self instVarNames includes: aString)
		ifFalse: [self error: aString , ' is not one of my instance variables'].
	newArray := self instVarNames copyWithout: aString.
	newString := ''.
	newArray do: [:aString2 | newString := aString2 , ' ' , newString].
	self instanceVariableNames: newString
]

{ #category : #'class hierarchy' }
Metaclass >> removeSubclass: aClass [
	"Do nothing."
]

{ #category : #'pool variables' }
Metaclass >> sharedPools [
	^OrderedCollection new.
]

{ #category : #'initialize-release' }
Metaclass >> slots: slotsDefinition [ 
	"Declare additional named variables for my instance."
	| theClass |
	theClass := self theNonMetaClass.
	
	theClass := theClass classInstaller make: [ :builder |
		builder
			superclass: theClass superclass;
			name: theClass name;
			layoutClass: theClass classLayout class;
			slots: theClass slots;
			sharedVariablesFromString: theClass classVariablesString;
			sharedPools: theClass sharedPoolsString;
			traitComposition: theClass traitComposition;
			category: theClass category asString;
			classSlots: slotsDefinition].
	^ theClass theMetaClass
]

{ #category : #accessing }
Metaclass >> soleInstance [
	"The receiver has only one instance. Answer it."

	^thisClass
]

{ #category : #'class hierarchy' }
Metaclass >> subclasses [
	"Answer the receiver's subclasses."
	
	self isMetaclassOfClassOrNil ifTrue: [ ^ #() ].
	
	^ self theNonMetaClass subclasses collect: [ :aSubclass | aSubclass theMetaClass ].
]

{ #category : #'class hierarchy' }
Metaclass >> subclassesDo: aBlock [
	"Evaluate aBlock for each of the receiver's immediate subclasses."
	
	self isMetaclassOfClassOrNil ifTrue: [ ^ self ].
	
	self theNonMetaClass subclasses do: [ :each | aBlock value: each theMetaClass ]
]

{ #category : #accessing }
Metaclass >> traitComposition [

	<localMetaclassMethod> 
    "This method is supposed to be local in Metaclass because of a good reason.
    We use this pragma to test if Metaclass does not contain some accidental 
    local selectors."

	traitComposition ifNil: [traitComposition := TraitComposition new].
	^traitComposition
]

{ #category : #accessing }
Metaclass >> traitComposition: aTraitComposition [

	<localMetaclassMethod> 
    "This method is supposed to be local in Metaclass because of a good reason.
    We use this pragma to test if Metaclass does not contain some accidental 
    local selectors."

	traitComposition := aTraitComposition
]

{ #category : #composition }
Metaclass >> uses: aTraitCompositionOrArray [
	| copyOfOldTrait newComposition |
	copyOfOldTrait := self copy.
	newComposition := aTraitCompositionOrArray asTraitComposition.
	self assertConsistantCompositionsForNew: newComposition.
	self setTraitComposition: newComposition.
	SystemAnnouncer uniqueInstance
		traitDefinitionChangedFrom: copyOfOldTrait to: self;
		classModificationAppliedTo: self
]

{ #category : #'initialize-release' }
Metaclass >> uses: aTraitCompositionOrArray instanceVariableNames: instVarString [ 
	| newComposition newMetaClass copyOfOldMetaClass |
	
	copyOfOldMetaClass := self copy.
	newMetaClass := self instanceVariableNames: instVarString.
	
	newComposition := aTraitCompositionOrArray asTraitComposition.
	newMetaClass assertConsistantCompositionsForNew: newComposition.
	newMetaClass setTraitComposition: newComposition.
	
	SystemAnnouncer uniqueInstance
		classDefinitionChangedFrom: copyOfOldMetaClass to: newMetaClass
]

{ #category : #'initialize-release' }
Metaclass >> uses: aTraitCompositionOrArray slots: slotArray [ 
	| newComposition newMetaClass copyOfOldMetaClass |
	
	copyOfOldMetaClass := self copy.
	newMetaClass := self slots: slotArray.
	
	newComposition := aTraitCompositionOrArray asTraitComposition.
	newMetaClass assertConsistantCompositionsForNew: newComposition.
	newMetaClass setTraitComposition: newComposition.
	
	SystemAnnouncer uniqueInstance
		classDefinitionChangedFrom: copyOfOldMetaClass to: newMetaClass
]

{ #category : #copying }
Metaclass >> veryDeepCopyWith: deepCopier [

	<localMetaclassMethod> 
    "This method is supposed to be local in Metaclass because of a good reason.
    We use this pragma to test if Metaclass does not contain some accidental 
    local selectors."

	"Return self.  Must be created, not copied.  Do not record me."
]

{ #category : #compiling }
Metaclass >> wantsChangeSetLogging [
	"Answer whether code submitted for the receiver should be remembered by the changeSet mechanism.The metaclass follows the rule of the class itself."

	^ self theNonMetaClass wantsChangeSetLogging
]

{ #category : #compiling }
Metaclass >> wantsRecompilationProgressReported [
	"The metaclass follows the rule of the class itself."

	^ self theNonMetaClass wantsRecompilationProgressReported
]
