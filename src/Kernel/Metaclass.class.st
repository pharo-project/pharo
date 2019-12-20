"
My instances add instance-specific behavior to various class-describing objects in the system. This typically includes messages for initializing class variables and instance creation messages particular to a class. There is only one instance of a particular Metaclass, namely the class which is being described. A Metaclass shares the class variables of its instance.
	
[Subtle] In general, the superclass hierarchy for metaclasses parallels that for classes. Thus,
	Integer superclass == Number, and
	Integer class superclass == Number class.
However there is a singularity at Object. Here the class hierarchy terminates, but the metaclass hierarchy must wrap around to Class, since ALL metaclasses are subclasses of Class. Thus,
	Object superclass == nil, and
	Object class superclass == Class.
"
Class {
	#name : #Metaclass,
	#superclass : #ClassDescription,
	#instVars : [
		'thisClass'
	],
	#category : #'Kernel-Classes'
}

{ #category : #compiling }
Metaclass >> acceptsLoggingOfCompilation [
	"Answer whether the receiver's method submisions and class defintions should be logged to the changes file and to the current change set.  The metaclass follows the rule of the class itself."

	^ self instanceSide acceptsLoggingOfCompilation
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

	^self instanceSide addClassSlot: aClassSlot
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

{ #category : #compiling }
Metaclass >> binding [
	"return an association that can be used as the binding
	 To share it between methods, reuse an existing one if possible"
	^self methodDict 
		ifEmpty: [LiteralVariable key: nil value: self]
		ifNotEmpty: [:dict | dict anyOne classBinding]
]

{ #category : #compiling }
Metaclass >> bindingOf: varName [

	^self instanceSide classBindingOf: varName
]

{ #category : #accessing }
Metaclass >> category [
	^ self instanceSide category
]

{ #category : #'pool variables' }
Metaclass >> classPool [
	"Answer the dictionary of class variables."

	^self instanceSide classPool
]

{ #category : #'accessing parallel hierarchy' }
Metaclass >> classSide [
	"Return the metaclass of the couple class/metaclass. Useful to avoid explicit test."
	"Point classSide >>> Point class"
	"Point class classSide >>> Point class"
	
	^ self
]

{ #category : #'accessing instances and variables' }
Metaclass >> classVarNames [
	"Answer the names of the class variables defined in the receiver's instance."
	
	^self instanceSide 
		ifNil: [ #() ]
		ifNotNil: [ :class | class classVarNames ]
]

{ #category : #'class variables' }
Metaclass >> classVariables [
	"Answer the class variables defined in the receiver's instance."

	^ self instanceSide
		ifNil: [ #() ]
		ifNotNil: [ :class | class classVariables ]
]

{ #category : #'file in/out' }
Metaclass >> definition [
	"Refer to the comment in ClassDescription|definition."

	(self slotsNeedFullDefinition or: [ Slot showSlotClassDefinition ]) ifTrue: [ 
		^ self definitionWithSlots ].

	^ self definitionWithoutSlots
]

{ #category : #'file in/out' }
Metaclass >> definitionWithSlots [
	"Refer to the comment in ClassDescription|definition."

	^ String streamContents: [:stream |
		stream print: self.
		stream
			crtab;
			nextPutAll: 'slots: ';
			nextPutAll: self slotDefinitionString ]
]

{ #category : #'file in/out' }
Metaclass >> definitionWithoutSlots [

	^ String streamContents: [:stream |
		stream print: self.
		stream
			crtab;
			nextPutAll: 'instanceVariableNames: ';
			store: self instanceVariablesString ]
]

{ #category : #accessing }
Metaclass >> environment [

	^thisClass environment
]

{ #category : #'accessing parallel hierarchy - deprecated' }
Metaclass >> hasClassSide [
	^ false
]

{ #category : #initialize }
Metaclass >> initializeFrom: anotherClassTrait [
	self traitComposition: self traitComposition copyTraitExpression.
	self methodDict: self methodDict copy.
	self basicOrganization: self organization copy.
]

{ #category : #compiling }
Metaclass >> innerBindingOf: varName [

	^self instanceSide innerBindingOf: varName
]

{ #category : #'accessing parallel hierarchy' }
Metaclass >> instanceSide [
	"Return the class of the couple class/metaclass. Useful to avoid explicit test."
	"Point instanceSide >>> Point"
	"Point class instanceSide >>> Point"
	
	^ self soleInstance
]

{ #category : #testing }
Metaclass >> isAnonymous [
	^self soleInstance isAnonymous 
]

{ #category : #testing }
Metaclass >> isClass [

	^ true
]

{ #category : #testing }
Metaclass >> isMeta [
	^ true
]

{ #category : #'class hierarchy' }
Metaclass >> isMetaclassOfClassOrNil [
	
	^ self instanceSide 
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

	^ thisClass ifNil: [ 'a Metaclass' ] ifNotNil: [ thisClass name asString , ' class' ]
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

	^self instanceSide newAnonymousSubclass class
]

{ #category : #'class hierarchy' }
Metaclass >> obsoleteSubclasses [
	"Answer the receiver's subclasses."

	self isMetaclassOfClassOrNil ifTrue: [ ^ #() ].
	^ self instanceSide obsoleteSubclasses collect: [ :aSubclass | aSubclass classSide ]
]

{ #category : #'file in/out' }
Metaclass >> oldDefinition [
	"Refer to the comment in ClassDescription|definition.
	 This method is for compatibility with Class>>#oldDefinition"

	^ self definition
]

{ #category : #compiling }
Metaclass >> possibleVariablesFor: misspelled continuedFrom: oldResults [

	^ self instanceSide possibleVariablesFor: misspelled continuedFrom: oldResults

]

{ #category : #copying }
Metaclass >> postCopy [
	"Don't share the reference to the sole instance."

	super postCopy.
	self classLayout: (layout copy host: self).
	thisClass := nil.
]

{ #category : #'instance variables' }
Metaclass >> removeSlot: aClassSlot [

	^self instanceSide removeClassSlot: aClassSlot
]

{ #category : #'class hierarchy' }
Metaclass >> removeSubclass: aClass [
	"Do nothing."
]

{ #category : #'pool variables' }
Metaclass >> sharedPools [
	^OrderedCollection new.
]

{ #category : #initialization }
Metaclass >> slots: slotCollection [ 

	| theClass |
	theClass := self instanceSide.
		
	theClass := theClass classInstaller update: theClass to: [ :builder |
		builder
			fillFor: theClass;
			classSlots: slotCollection ].
	^ theClass classSide
]

{ #category : #accessing }
Metaclass >> soleInstance [
	"The receiver has only one instance. Answer it."

	^thisClass
]

{ #category : #compiling }
Metaclass >> sourceCodeTemplate [
	"Answer an expression to be edited and evaluated in order to define 
	methods in this class or trait."

	^ 'messageSelectorAndArgumentNames
	"comment stating purpose of class-side message"
	"scope: class-variables  &  class-instance-variables"	
			
	| temporary variable names |
	statements'
]

{ #category : #'class hierarchy' }
Metaclass >> subclasses [
	"Answer the receiver's subclasses."
	
	self isMetaclassOfClassOrNil ifTrue: [ ^ #() ].
	^ self instanceSide subclasses collect: [ :aSubclass | aSubclass classSide ].
]

{ #category : #'class hierarchy' }
Metaclass >> subclassesDo: aBlock [
	"Evaluate aBlock for each of the receiver's immediate subclasses."
	
	self isMetaclassOfClassOrNil ifTrue: [ ^ self ].
	self instanceSide subclasses do: [ :each | aBlock value: each classSide ]
]

{ #category : #copying }
Metaclass >> veryDeepCopyWith: deepCopier [

	"Return self.  Must be created, not copied.  Do not record me."
]

{ #category : #compiling }
Metaclass >> wantsChangeSetLogging [
	"Answer whether code submitted for the receiver should be remembered by the changeSet mechanism.The metaclass follows the rule of the class itself."

	^ self instanceSide wantsChangeSetLogging
]
