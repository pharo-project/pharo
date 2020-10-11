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



### Concerns 
Note that sending a message to the superclass is close to subclass: and it lets the class selects 
a class definition parser if the syntax should be extended. 
In addition having a binary message makes () unneccessary.


### Class side definition 
In Pharo, a metaclass cannot be created in isolation (yes we can programmatically) so the class side definition is the following one. But we need more investigation on this point.

``` 
	Point class
		uses: TraitX;
		slots: { foo }
```	



"
Class {
	#name : #CDFluidClassDefinitionParser,
	#superclass : #CDAbstractClassDefinitionParser,
	#instVars : [
		'subclass'
	],
	#category : #ClassParser
}

{ #category : #parsing }
CDFluidClassDefinitionParser class >> fromASTNode: aNode [ 
	
	^ self new parseRootNode: aNode
]

{ #category : #parsing }
CDFluidClassDefinitionParser class >> parse: aString [ 
	
	^ self new parse: aString
]

{ #category : #internal }
CDFluidClassDefinitionParser >> beClassSideTraitDefinition [
		
	classDefinition := CDClassSideTraitDefinitionNode new
]

{ #category : #internal }
CDFluidClassDefinitionParser >> beTraitDefinition [
		
	classDefinition := CDTraitDefinitionNode new
]

{ #category : #'handling  nodes' }
CDFluidClassDefinitionParser >> handleClassAndSuperclassOf: aNode [

	(self isTraitDefinition: aNode receiver)
		ifTrue: [ self handleClassName: aNode receiver arguments first. ^ self ].
	(self isTraitClassSideDefinition: aNode receiver)
		ifTrue: [ self handleClassName: aNode receiver receiver. ^ self].
	
	(self isInstanceSideDefinition: aNode receiver)
		ifTrue: [ 
			self handleSuperclassNode: aNode receiver receiver.
			self handleClassName: aNode receiver arguments first ]
		ifFalse: [ self handleClassName: aNode receiver receiver ].
]

{ #category : #'handling  nodes' }
CDFluidClassDefinitionParser >> handleLayout: aNode [

	classDefinition classKind: aNode binding value kind
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
CDFluidClassDefinitionParser >> handleSlotNode: aRBMessageNode [

	"what ugly method! Pay attention start and stop should be tested and probably adjusted!"

	"when a slot is just 'inst'"

	aRBMessageNode isLiteralNode ifTrue: [ 
		| slot |
		slot := self slotNodeClass
			        node: aRBMessageNode
			        name: aRBMessageNode value
			        slotClassName: #InstanceVariableSlot
			        initializationMessage: self slotInitializationNodeClass new
			        start: aRBMessageNode start
			        stop: aRBMessageNode stop.
		classDefinition addSlot: slot.
		^ self ].

	aRBMessageNode selector = '=>' ifTrue: [ 
		aRBMessageNode arguments first isMessage
			ifFalse: [ "when a slot is just 'inst' => InstanceVariableSlot."
				aRBMessageNode arguments first isLiteralVariable ifTrue: [ 
					| slot |
					slot := self slotNodeClass
						        node: aRBMessageNode
						        name: aRBMessageNode receiver value
						        slotClassName: aRBMessageNode arguments first name
						        initializationMessage:
						        self slotInitializationNodeClass new
						        start: aRBMessageNode start
						        stop: aRBMessageNode stop.
					classDefinition addSlot: slot.
					^ self ]
				"we will have to do something for example if we have 
				#x => 2" ]
			ifTrue: [ "when a slot is 'inst' => (InstanceVariableSlot default: 5)."
				| slot slotDefNode |
				slotDefNode := aRBMessageNode.
				slot := self slotNodeClass
					        node: aRBMessageNode
					        name: slotDefNode receiver value
					        slotClassName: slotDefNode arguments first receiver name
					        initializationMessage: (self slotInitializationNodeClass
							         selectorParts:
							         aRBMessageNode arguments first selectorParts
							         argumentParts:
							         aRBMessageNode arguments first arguments)
					        start: aRBMessageNode start
					        stop: aRBMessageNode stop.
				classDefinition addSlot: slot.
				^ self ] ]
]

{ #category : #'handling  nodes' }
CDFluidClassDefinitionParser >> handleSlotsNodesFromArrayNode: aRBArrayNode [
	
	aRBArrayNode statements do: [ :slotStatement |
		self handleSlotNode: slotStatement ]
]

{ #category : #'handling  nodes' }
CDFluidClassDefinitionParser >> handleSuperclassNode: aSuperclassNode [
	| aSuperclassName superclassNode |
	aSuperclassName := aSuperclassNode isLiteralNode
		ifTrue: [ nil ]
		ifFalse: [ aSuperclassNode binding value asString asSymbol ].
	superclassNode := self classNameNodeClass new
		originalNode: aSuperclassNode;
		className: aSuperclassName.
	classDefinition
		superclassName: aSuperclassName
		astNode: superclassNode
]

{ #category : #'handling  nodes' }
CDFluidClassDefinitionParser >> handleTag: aNode [

	classDefinition tag: (CDClassTagNode new name: aNode value)
]

{ #category : #'handling  nodes' }
CDFluidClassDefinitionParser >> handleTraitUsesFromNode: aNode [
	
	| traitComposition |

	aNode isDynamicArray and: [aNode allStatements ifEmpty: [ ^self  ]].
 	traitComposition := CDTraitCompositionBuilder new buildFrom: aNode.	
	classDefinition traitDefinition: traitComposition.
]

{ #category : #testing }
CDFluidClassDefinitionParser >> isInstanceSideDefinition: aRBMessageNode [
	"Returns whether the definition is a class or instance side definition. Superclass < #myObject. Trait << are handled prior to this method."
	
	^ aRBMessageNode isMessage and: [ aRBMessageNode selector = #< ]
	
]

{ #category : #testing }
CDFluidClassDefinitionParser >> isTraitClassSideDefinition: aRBMessageNode [
	"Based on the message structure, returns whether the definition is a trait definition."

	^ aRBMessageNode isMessage and: [ aRBMessageNode selector = #classTrait ]
	
]

{ #category : #testing }
CDFluidClassDefinitionParser >> isTraitDefinition: aRBMessageNode [
	"Returns whether the definition is a trait definition, i.e., Trait << #Tbehavior."

	^ aRBMessageNode isMessage and: [ aRBMessageNode selector = #<< ]
	
]

{ #category : #'parsing-internal' }
CDFluidClassDefinitionParser >> parseRootNode: expressionTree [
	
	expressionTree extractClassDefinitionFor: self. 
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
	aString =	 #uses:
		ifTrue: [ ^ self handleTraitUsesFromNode: aNode ].
	aString =	 #sharedPools:
		ifTrue: [ ^ self handleSharedPoolsFromNode: aNode ].
	
	CDUnrecognizedClassPart new
		classDefinition: self; 
		unrecognizedPart: aString;
		signal
]

{ #category : #internal }
CDFluidClassDefinitionParser >> setAdequateClassDefinitionFrom: expressionTree [
	"Set a class, trait, class side trait or metaclass definition as definition object."
	
	(self isTraitDefinition: expressionTree)
		ifTrue: [ self beTraitDefinition. ^ self ].
	(self isTraitClassSideDefinition: expressionTree)	
		ifTrue: [ self beClassSideTraitDefinition. ^ self ].
	super setAdequateClassDefinitionFrom: expressionTree
]

{ #category : #'parsing-internal' }
CDFluidClassDefinitionParser >> visitCascadeNode: aRBCascadeNode [ 
	
	self handleClassAndSuperclassOf: aRBCascadeNode.	
	aRBCascadeNode messages do: 
		[:msg | msg selectorParts
					with: msg arguments
					do: [ :selectorPart :argument |
			self parseSelectorPart: selectorPart withArgument: argument ] ]
]

{ #category : #'parsing-internal' }
CDFluidClassDefinitionParser >> visitMessageNode: aRBMessageNode [

	"Object < #Point
			package: ''Kernel-BasicObjects'''.
	or 
	Object < #Point
			uses: TraitX;
			slots: { foo };
			package: ''Kernel-BasicObjects'''
	
	or 
	Point class
		uses: TraitX;
		slots: { foo }
		
	With this one we do not pass via the builder.
	"		

	self handleClassAndSuperclassOf: aRBMessageNode.
	
	
	aRBMessageNode selectorParts
		with: aRBMessageNode arguments
		do: [ :selectorPart :argument |
			self parseSelectorPart: selectorPart withArgument: argument ]
]
