"
I'm parser for the new class syntax. 


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
	#name : #CDFluidClassDefinitionParser,
	#superclass : #CDAbstractClassDefinitionParser,
	#instVars : [
		'context',
		'classNameNode',
		'superclassNode'
	],
	#category : #'ClassParser-Parser'
}

{ #category : #parsing }
CDFluidClassDefinitionParser class >> fromASTNode: aNode [ 
	
	^ self new parseRootNode: aNode
]

{ #category : #'handling  nodes' }
CDFluidClassDefinitionParser >> handleClassAndSuperclassOf: aNode [

	superclassNode ifNotNil: [ self handleSuperclassNode: superclassNode ].
	self handleClassName: classNameNode.

]

{ #category : #internal }
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

{ #category : #'handling  nodes' }
CDFluidClassDefinitionParser >> handleLayout: aNode [
	classDefinition layoutClass: aNode binding value
]

{ #category : #'metaclass ' }
CDFluidClassDefinitionParser >> handleMetaclassNameFromCascade: aRBCascadeNode [ 

	| className classNode node |
	node := aRBCascadeNode receiver arguments first. 
	className := node receiver binding value class name.
	classNode := self classNameNodeClass new
		originalNode: node;
		className: className.
	classDefinition className: node astNode: classNode
]

{ #category : #'metaclass ' }
CDFluidClassDefinitionParser >> handleMetaclassNameFromMessage: aNode [ 

	| className classNode node |
	node := aNode arguments first receiver. 
	className := node binding value class name.
	classNode := self classNameNodeClass new
		originalNode: node;
		className: className.
	classDefinition className: node astNode: classNode
]

{ #category : #'handling  nodes' }
CDFluidClassDefinitionParser >> handleSharedPoolsFromNode: aNode [
	| sharedPoolNodes |
	sharedPoolNodes := aNode children
		collect: [ :aPoolNode | 
			self sharedPoolNodeClass
				node: aPoolNode
				name: aPoolNode name
				start: aPoolNode start 
				stop: aPoolNode start + aPoolNode name size
			].
	classDefinition sharedPools: sharedPoolNodes
]

{ #category : #'handling  nodes' }
CDFluidClassDefinitionParser >> handleSharedVariableNames: aNode [ 
	
	| slotNodes classVariablesString |
	classVariablesString := aNode sourceCode.
	slotNodes := aNode statements
		collect: [ :literalNode | 		
			| start literalName |
			literalName := literalNode value.
			start := aNode start + (classVariablesString findString: literalName).
			CDSharedVariableNode
				node: aNode
				name: literalName
				slotClassName: 'ClassVariableSlot'
				start: start
				stop: start + literalNode size ].
	classDefinition sharedSlots: slotNodes
]

{ #category : #'handling  nodes' }
CDFluidClassDefinitionParser >> handleSuperclassNode: aSuperclassNode [
	| aSuperclassName newSuperclassNode |

	aSuperclassName := aSuperclassNode isLiteralNode
		ifTrue: [ nil ]
		ifFalse: [ aSuperclassNode binding value asString asSymbol ].
	newSuperclassNode := self classNameNodeClass new
		originalNode: aSuperclassNode;
		className: aSuperclassName.
	classDefinition
		superclassName: aSuperclassName
		astNode: newSuperclassNode
]

{ #category : #'handling  nodes' }
CDFluidClassDefinitionParser >> handleTag: aNode [

	classDefinition tag: (CDClassTagNode new name: aNode value)
]

{ #category : #'parsing-internal' }
CDFluidClassDefinitionParser >> parseRootNode: expressionTree [
	
	| searcher |
	searcher := RBParseTreeSearcher new.
	searcher
		matches: 'Trait << `#name' do: [:aNode :answer | 
			classNameNode := searcher variableNamed: '`#name'.
			self beTraitDefinition ];
		matches: 'Trait << `name classTrait' do: [:aNode :answer | 
			classNameNode := searcher variableNamed: '`name'.
			self beClassSideTraitDefinition ];
		matches: '`superclass << `#name' do: [:aNode :answer | 
			classNameNode := searcher variableNamed: '`#name'.			
			superclassNode := searcher variableNamed: '`superclass'.
			self beClassDefinition ];
		matches: '`superclass class << `name class' do: [:aNode :answer | 
			classNameNode := searcher variableNamed: '`name'.			
			self beMetaclassDefinition ].
		
	searcher executeTree: expressionTree.
	
	classDefinition originalNode: expressionTree.
	expressionTree acceptVisitor: self.
	^ classDefinition
]

{ #category : #'parsing-internal' }
CDFluidClassDefinitionParser >> parseSelectorPart: aString withArgument: aNode [ 
	"We could do this with reflection, or with a dictionary and closures.
	I chose to use a series of if for readability only."
	
	aString =	 #slots:
		ifTrue: [ ^ self handleSlotsNodesFromArrayNode: aNode ].
	aString =	 #sharedVariables: 
		ifTrue: [ ^ self handleSharedVariableNames: aNode ].	
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

{ #category : #'parsing-internal' }
CDFluidClassDefinitionParser >> visitCascadeNode: aRBCascadeNode [ 
	"See class comment. Here we handle the cascade version of the class definition."
	" 
	Object << #Point
			trait: TraitX;
			slots: { #foo };
			package: ''Kernel-BasicObjects'''
	"
	
	self handleClassAndSuperclassOf: aRBCascadeNode.	
	aRBCascadeNode messages do: 
		[:msg | msg selectorParts
					with: msg arguments
					do: [ :selectorPart :argument |
			self parseSelectorPart: selectorPart withArgument: argument ] ]
]

{ #category : #'parsing-internal' }
CDFluidClassDefinitionParser >> visitMessageNode: aRBMessageNode [
	"See class comment. Here we handle the cascade version of the class definition."
	"
	Object << #Point
			package: ''Kernel-BasicObjects'''
	"		

	self handleClassAndSuperclassOf: aRBMessageNode.
	
	aRBMessageNode arguments ifEmpty: [ ^ self ].
	aRBMessageNode selectorParts
		with: aRBMessageNode arguments
		do: [ :selectorPart :argument |
			self parseSelectorPart: selectorPart withArgument: argument ]
]
