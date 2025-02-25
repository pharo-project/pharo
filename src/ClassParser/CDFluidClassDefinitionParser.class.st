"
I'm parser for the fluid class syntax. 


### The new syntax for class definition (just for the class and not for the methods) is

```
Superclass < #MyObject
	uses: #MyTrait;
	slots: { #a. #b };
	sharedVariables: { #A. #B };
	tags: {'tag2' . 'Core'} ;
	layout: VariableLayout; 
	sharedPools: { #TextConstants } ;
	package: #MyPackage
```
Pay attention all the messages should be keyword-based.

The minimal class definition is the following one: 

```
Superclass < #MyObject
	package: #MyPackage
```

For the implementors, we see that we should either handle a simple message composition or a cascade. 

### Design Concerns 
Note that sending a message to the superclass is close to subclass: and it lets the class selects 
a class definition parser if the syntax should be extended. 
In addition having a binary message makes () unneccessary.


"
Class {
	#name : 'CDFluidClassDefinitionParser',
	#superclass : 'Object',
	#instVars : [
		'classNameNode',
		'superclassNode',
		'classDefinition'
	],
	#classInstVars : [
		'unrestrictedVariableDefinitions'
	],
	#category : 'ClassParser-Parser',
	#package : 'ClassParser',
	#tag : 'Parser'
}

{ #category : 'parsing' }
CDFluidClassDefinitionParser class >> fromASTNode: aNode [

	^ self new parseRootNode: aNode
]

{ #category : 'instance creation' }
CDFluidClassDefinitionParser class >> parse: aString [

	^ self new parse: aString
]

{ #category : 'settings' }
CDFluidClassDefinitionParser class >> settingsOn: aBuilder [
	
	<systemsettings>
	(aBuilder setting: #unrestrictedVariableDefinitions)
		parent: #codeBrowsing;
		default: false;
		label: 'Unrestricted Slots/Class Variable Definitions';
		description: 'EXPERIMENTAL: If true, the system will allow any expression as 
a Variable Definition (for Slots and Classvariables) that evaluates to a valid instance. 
		
WARNING1: you have to load all the Variable implementation classes *before* any users. 
WARNING2: the definition will be loaded by *unchecked* evaluation';
		target: self.
]

{ #category : 'settings' }
CDFluidClassDefinitionParser class >> unrestrictedVariableDefinitions [
	^ unrestrictedVariableDefinitions ifNil: [ false ]
]

{ #category : 'settings' }
CDFluidClassDefinitionParser class >> unrestrictedVariableDefinitions: aBoolean [
	unrestrictedVariableDefinitions := aBoolean
]

{ #category : 'internal' }
CDFluidClassDefinitionParser >> beClassDefinition [

	classDefinition := CDClassDefinitionNode new
]

{ #category : 'internal' }
CDFluidClassDefinitionParser >> beClassSideTraitDefinition [

	classDefinition := CDClassSideTraitDefinitionNode new
]

{ #category : 'internal' }
CDFluidClassDefinitionParser >> beMetaclassDefinition [

	classDefinition := CDMetaclassDefinitionNode new
]

{ #category : 'internal' }
CDFluidClassDefinitionParser >> beTraitDefinition [

	classDefinition := CDTraitDefinitionNode new
]

{ #category : 'private - class factory' }
CDFluidClassDefinitionParser >> classNameNodeClass [
	^ CDClassNameNode
]

{ #category : 'parsing' }
CDFluidClassDefinitionParser >> handleClassAndSuperclassOf: aNode [

	superclassNode ifNotNil: [ self handleSuperclassNode: superclassNode ].
	self handleClassName: classNameNode
]

{ #category : 'parsing' }
CDFluidClassDefinitionParser >> handleClassName: aNode [
	| className classNode |
	className := (aNode isVariable
		ifTrue: [ aNode name ]
		ifFalse: [ aNode value ]) asSymbol.
	classNode := self classNameNodeClass new
		originalNode: aNode;
		className: className.
	classDefinition className: className astNode: classNode
]

{ #category : 'parsing' }
CDFluidClassDefinitionParser >> handleInstanceVariablesFromNode: aNode [
	| slots slotNodes instanceVariablesString |
	instanceVariablesString := aNode value.
	slots := instanceVariablesString substrings.
	slotNodes := slots
		collect: [ :slotName |
			| start |
			start := aNode start
				+ (instanceVariablesString findString: slotName).
			CDSlotNode new
				node: aNode;
				name: slotName;
				variableClassName: #InstanceVariableSlot;
				start: start;
				stop: start + slotName size ].
	classDefinition slots: slotNodes
]

{ #category : 'parsing' }
CDFluidClassDefinitionParser >> handleLayout: aNode [
	classDefinition layoutClass: aNode binding value
]

{ #category : 'parsing' }
CDFluidClassDefinitionParser >> handleMetaclassName: aNode [
	"we are in situation ClassX class << ClassZ class
				slots: {xxxx} so grab the class"

	aNode handleMetaclassName: self
]

{ #category : 'parsing' }
CDFluidClassDefinitionParser >> handleMetaclassNameFromCascade: aCascadeNode [

	| className classNode node |
	node := aCascadeNode receiver arguments first.
	className := node receiver binding value class name.
	classNode := self classNameNodeClass new
		originalNode: node;
		className: className.
	classDefinition className: node astNode: classNode
]

{ #category : 'parsing' }
CDFluidClassDefinitionParser >> handleMetaclassNameFromMessage: aNode [

	| className classNode node |
	node := aNode arguments first receiver.
	className := node binding value class name.
	classNode := self classNameNodeClass new
		originalNode: node;
		className: className.
	classDefinition className: node astNode: classNode
]

{ #category : 'parsing' }
CDFluidClassDefinitionParser >> handlePackage: aNode [

	classDefinition packageNameNode: aNode astNode: aNode
]

{ #category : 'parsing' }
CDFluidClassDefinitionParser >> handleSharedPoolsFromNode: aNode [
	| sharedPoolNodes |
	sharedPoolNodes := aNode children
		collect: [ :aPoolNode |
			self sharedPoolNodeClass
				node: aPoolNode
				name: aPoolNode value name
				start: aPoolNode start
				stop: aPoolNode start + aPoolNode value size
			].
	classDefinition sharedPools: sharedPoolNodes
]

{ #category : 'parsing - variables' }
CDFluidClassDefinitionParser >> handleSharedVarableNodeSimple: slotDefNode [

	"when a class variable is just #Name"

	^ CDSharedVariableNode new
		  node: slotDefNode;
		  name: slotDefNode value;
		  variableClassName: #ClassVariable;
		  start: slotDefNode start;
		  stop: slotDefNode stop
]

{ #category : 'parsing - variables' }
CDFluidClassDefinitionParser >> handleSharedVariabeNode: variableDefNode [
	 | variable |
	
	"when a class variable is just #ClassVars"
	variableDefNode isLiteralNode ifTrue: [ ^ classDefinition addSharedVariable: (self handleSharedVarableNodeSimple: variableDefNode)].
	
	"Setting, default off, to allow experiments"
	self class unrestrictedVariableDefinitions ifTrue: [ 
		^ classDefinition addSharedVariable: (self handleUnrestrictedSharedVariableDefinition: variableDefNode) ].
	
	"#ClassVar => SomeVar default: 5; default2: 4"
	variableDefNode isCascade ifTrue: [ variable := self handleSharedVariableNodeCascade: variableDefNode].
	"when a class var is just #var => SomeVar"
	(variableDefNode isMessage and: [ variableDefNode selector = '=>'  ])ifTrue: [  variable := self handleSharedVariableNodeSimpleClass: variableDefNode].
	"When a class variable is #Var => SomeVar default: 5"
	(variableDefNode isMessage and: [ variableDefNode selector ~= '=>'  ]) ifTrue:  [ variable := self handleSharedVariableNodeSimpleClassArgument: variableDefNode].

	variable ifNil: [ ^self error: 'Variable definion can not be parsed' ].
	classDefinition addSharedVariable: variable
]

{ #category : 'parsing' }
CDFluidClassDefinitionParser >> handleSharedVariableNames: aNode [

	| slots slotNodes classVariablesString |
	classVariablesString := aNode value.
	slots := classVariablesString substrings.
	slotNodes := slots
		collect: [ :slotName | | start |
			start := aNode start + (classVariablesString findString: slotName).
			CDSharedVariableNode
				node: aNode
				name: slotName
				slotClassName: #ClassVariable
				start: start
				stop: start + slotName size ].
	classDefinition sharedVariables: slotNodes
]

{ #category : 'parsing - variables' }
CDFluidClassDefinitionParser >> handleSharedVariableNodeCascade: slotDefNode [

	"#ClassVar => SomeVar default: 5; default2: 4"

	^ CDSharedVariableNode new
		  node: slotDefNode;
		  name: slotDefNode receiver receiver value;
		  variableClassName: slotDefNode receiver arguments first value name;
		  start: slotDefNode start;
		  stop: slotDefNode stop
]

{ #category : 'parsing - variables' }
CDFluidClassDefinitionParser >> handleSharedVariableNodeSimpleClass: slotDefNode [

	"when a calss var is just #var => SomeVar"

	^ CDSharedVariableNode new
		  node: slotDefNode;
		  name: slotDefNode receiver value;
		  variableClassName: slotDefNode arguments first name;
		  start: slotDefNode start;
		  stop: slotDefNode stop
]

{ #category : 'parsing - variables' }
CDFluidClassDefinitionParser >> handleSharedVariableNodeSimpleClassArgument: slotDefNode [

	"When a class variable is #Var => SomeVar default: 5"

	^ CDSharedVariableNode new
		  node: slotDefNode;
		  name: slotDefNode receiver receiver value;
		  variableClassName: slotDefNode receiver arguments first name;
		  start: slotDefNode start;
		  stop: slotDefNode stop
]

{ #category : 'parsing - variables' }
CDFluidClassDefinitionParser >> handleSharedVariableNodesFromArrayNode: anArrayNode [

	anArrayNode statements do: [ :slotStatement |
		self handleSharedVariabeNode: slotStatement ]
]

{ #category : 'parsing - variables' }
CDFluidClassDefinitionParser >> handleSlotNode: slotDefNode [
	 | slot |
	
	"when a slot is just #inst"
	slotDefNode isLiteralNode ifTrue: [
		^ classDefinition addSlot: ( self handleSlotNodeSimple: slotDefNode)].
	
	"Setting, default off, to allow experiments"
	self class unrestrictedVariableDefinitions ifTrue: [ 
		^ classDefinition addSlot: (self handleUnrestrictedSlotDefinition: slotDefNode) ].
	
	"#inst => InstanceVariableSlot default: 5; default2: 4"
	slotDefNode isCascade ifTrue: [ slot := self handleSlotNodeCascade: slotDefNode].
	"when a slot is just #inst => InstanceVariableSlot."
	(slotDefNode isMessage and: [ slotDefNode selector = '=>'  ])ifTrue: [  slot := self handleSlotNodeSimpleClass: slotDefNode].
	"when a slot is #inst => InstanceVariableSlot default: 5."
	(slotDefNode isMessage and: [ slotDefNode selector ~= '=>'  ]) ifTrue:  [ slot := self handleSlotNodeSimpleClassArgument: slotDefNode].

	slot ifNil: [ ^self error: 'Slot definion can not be parsed' ].
	classDefinition addSlot: slot
]

{ #category : 'parsing - variables' }
CDFluidClassDefinitionParser >> handleSlotNodeCascade: slotDefNode [

	"#inst => InstanceVariableSlot default: 5; default2: 4"

	^ CDSlotNode new
		  node: slotDefNode;
		  name: slotDefNode receiver receiver value;
		  variableClassName: slotDefNode receiver arguments first value name;
		  start: slotDefNode start;
		  stop: slotDefNode stop
]

{ #category : 'parsing - variables' }
CDFluidClassDefinitionParser >> handleSlotNodeSimple: slotDefNode [

	"when a slot is just #inst"

	^ CDSlotNode new
		  node: slotDefNode;
		  name: slotDefNode value;
		  variableClassName: #InstanceVariableSlot;
		  start: slotDefNode start;
		  stop: slotDefNode stop
]

{ #category : 'parsing - variables' }
CDFluidClassDefinitionParser >> handleSlotNodeSimpleClass: slotDefNode [

	"when a variable is just #var => InstanceVariableSlot"

	^ CDSlotNode new
		  node: slotDefNode;
		  name: slotDefNode receiver value;
		  variableClassName: slotDefNode arguments first name;
		  start: slotDefNode start;
		  stop: slotDefNode stop
]

{ #category : 'parsing - variables' }
CDFluidClassDefinitionParser >> handleSlotNodeSimpleClassArgument: slotDefNode [

	"When a vaiable is #var => InstanceVariableSlot default: 5"

	^ CDSlotNode new
		  node: slotDefNode;
		  name: slotDefNode receiver receiver value;
		  variableClassName: slotDefNode receiver arguments first name;
		  start: slotDefNode start;
		  stop: slotDefNode stop
]

{ #category : 'parsing - variables' }
CDFluidClassDefinitionParser >> handleSlotsNodesFromArrayNode: anArrayNode [

	anArrayNode statements do: [ :slotStatement |
		self handleSlotNode: slotStatement ]
]

{ #category : 'parsing' }
CDFluidClassDefinitionParser >> handleSuperclassNode: aSuperclassNode [
	| aSuperclassName newSuperclassNode |

	aSuperclassName := aSuperclassNode isLiteralNode
		ifTrue: [ nil ]
		ifFalse: [aSuperclassNode name asSymbol ].
	newSuperclassNode := self classNameNodeClass new
		originalNode: aSuperclassNode;
		className: aSuperclassName.
	classDefinition
		superclassName: aSuperclassName
		astNode: newSuperclassNode
]

{ #category : 'parsing' }
CDFluidClassDefinitionParser >> handleTag: aNode [

	classDefinition tag: (CDClassTagNode new name: aNode value)
]

{ #category : 'parsing' }
CDFluidClassDefinitionParser >> handleTraitUsesFromNode: aNode [

	| traitComposition |
	aNode isDynamicArray and: [aNode allStatements ifEmpty: [ ^ self ]].
 	traitComposition := CDTraitCompositionBuilder new buildFrom: aNode.
	classDefinition traitDefinition: traitComposition
]

{ #category : 'parsing - variables' }
CDFluidClassDefinitionParser >> handleUnrestrictedSharedVariableDefinition: variableDefNode [

	"If the preference is enabled, allow unrestricted shared variable definitions. 
	TAKE CARE: There is no way to know the name nor the class without executing the code of the defintion.
	This means that a) code will be evaluated which could have side effects and b) you need to load the Variable class
	before any users, as an UndefinedClassVariable can not be created, as we do not know the name"

	| variableInstance |
	"create the instance of the Variable by evaluating the defintion, we use this to fill out the name and variableClassName"
	variableInstance := variableDefNode evaluate.

	^ CDSharedVariableNode new
		  node: variableDefNode;
		  name: variableInstance name;
		  variableClassName: variableInstance class name;
		  start: variableDefNode start;
		  stop: variableDefNode stop
]

{ #category : 'parsing - variables' }
CDFluidClassDefinitionParser >> handleUnrestrictedSlotDefinition: slotDefNode [

	"If the preference is enabled, allow unrestricted slot definitions. 
	TAKE CARE: There is no way to know the name nor the class witout executing the code of the defintion.
	This means that a) code will be evaluated which could have side effects and b) you need to load the Slot Class
	before any users, as an UndefinedSlot can not be created, as we do not know the name"

	| slotInstance |
	"create the instance of the slot for real, we use this to fill out the name and variableClassName"
	slotInstance := slotDefNode evaluate.

	^ CDSlotNode new
		  node: slotDefNode;
		  name: slotInstance name;
		  variableClassName: slotInstance class name;
		  start: slotDefNode start;
		  stop: slotDefNode stop
]

{ #category : 'parsing' }
CDFluidClassDefinitionParser >> parse: aString [
	| expressionTree |
	expressionTree := self parserClass parseExpression: aString.
	expressionTree doSemanticAnalysis.
	^ self parseRootNode: expressionTree
]

{ #category : 'parsing' }
CDFluidClassDefinitionParser >> parseRootNode: expressionTree [

	| searcher |
	searcher := OCParseTreeSearcher new.
	searcher
		matches: 'Trait << `#name' do: [ :aNode :answer |
			classNameNode := searcher variableNamed: '`#name'.
			self beTraitDefinition.
			classDefinition superclassName: #Trait ];
		matches: 'Trait << `name classTrait' do: [ :aNode :answer |
			classNameNode := searcher variableNamed: '`name'.
			self beClassSideTraitDefinition ];
		matches: '`superclass << `#name' do: [ :aNode :answer |
			classNameNode := searcher variableNamed: '`#name'.
			superclassNode := searcher variableNamed: '`superclass'.
			self beClassDefinition ];
		matches: 'nil << `#name' do: [ :aNode :answer |
			classNameNode := searcher variableNamed: '`#name'.
			self beClassDefinition.
			classDefinition superclassName: #nil ];
		matches: '`superclass class << `name class' do: [ :aNode :answer |
			classNameNode := searcher variableNamed: '`name'.
			self beMetaclassDefinition ].

	searcher executeTree: expressionTree.

	classDefinition originalNode: expressionTree.
	expressionTree acceptVisitor: self.
	^ classDefinition
]

{ #category : 'parsing' }
CDFluidClassDefinitionParser >> parseSelectorPart: aString withArgument: aNode [
	"We could do this with reflection, or with a dictionary and closures.
	I chose to use a series of if for readability only."

	aString =	 #slots:
		ifTrue: [ ^ self handleSlotsNodesFromArrayNode: aNode ].
	aString =	 #sharedVariables:
		ifTrue: [ ^ self handleSharedVariableNodesFromArrayNode: aNode ].
	aString =	 #package:
		ifTrue: [ ^ self handlePackage: aNode ].
	aString = #layout:
		ifTrue: [ ^ self handleLayout: aNode ].
	aString = #tag:
		ifTrue: [  ^self handleTag: aNode ].
	aString =	 #traits:
		ifTrue: [ ^ self handleTraitUsesFromNode: aNode ].
	aString =	 #sharedPools:
		ifTrue: [ ^ self handleSharedPoolsFromNode: aNode ].

	CDUnrecognizedClassPart new
		classDefinition: self;
		unrecognizedPart: aString;
		signal
]

{ #category : 'private - class factory' }
CDFluidClassDefinitionParser >> parserClass [
	^ OCParser
]

{ #category : 'private - class factory' }
CDFluidClassDefinitionParser >> sharedPoolNodeClass [
	^ CDSharedPoolNode
]

{ #category : 'private - class factory' }
CDFluidClassDefinitionParser >> slotNodeClass [
	^ CDSlotNode
]

{ #category : 'parsing' }
CDFluidClassDefinitionParser >> visitCascadeNode: aCascadeNode [
	"See class comment. Here we handle the cascade version of the class definition."
	"
	Object << #Point
			trait: TraitX;
			slots: { #foo };
			package: ''Kernel-BasicObjects'''
	"

	self handleClassAndSuperclassOf: aCascadeNode.
	aCascadeNode messages do:
		[:msg | msg selectorParts
					with: msg arguments
					do: [ :selectorPart :argument |
			self parseSelectorPart: selectorPart withArgument: argument ] ]
]

{ #category : 'parsing' }
CDFluidClassDefinitionParser >> visitMessageNode: aMessageNode [
	"See class comment. Here we handle the cascade version of the class definition."
	"
	Object << #Point
			package: ''Kernel-BasicObjects'''
	"

	self handleClassAndSuperclassOf: aMessageNode.

	aMessageNode arguments ifEmpty: [ ^ self ].
	aMessageNode selectorParts
		with: aMessageNode arguments
		do: [ :selectorPart :argument |
			self parseSelectorPart: selectorPart withArgument: argument ]
]
