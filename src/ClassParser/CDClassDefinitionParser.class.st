"
I use the default node representation of classes to parse class definition and  return a dedicated class tree.

The basic idea behing the implementation is to reuse as much as possible as the current parser infrastructure. This is why this class defines a visitMessageNode: method. 

Main public API is 
	- parse: aString
"
Class {
	#name : #CDClassDefinitionParser,
	#superclass : #Object,
	#instVars : [
		'classDefinition',
		'subclass'
	],
	#category : #'ClassParser-Model'
}

{ #category : #parsing }
CDClassDefinitionParser class >> fromASTNode: aNode [ 
	
	^ self new parseRootNode: aNode
]

{ #category : #parsing }
CDClassDefinitionParser class >> parse: aString [ 
	
	^ self new parse: aString
]

{ #category : #internal }
CDClassDefinitionParser >> beClassDefinition [
		
	classDefinition := CDClassDefinitionNode new
]

{ #category : #internal }
CDClassDefinitionParser >> beMetaclassDefinition [
		
	classDefinition := CDMetaclassDefinitionNode new
]

{ #category : #'private class factory' }
CDClassDefinitionParser >> classNameNodeClass [
	^ CDClassNameNode
]

{ #category : #accessing }
CDClassDefinitionParser >> classVariableNames: aNode [ 
	
	| slots slotNodes classVariablesString |
	classVariablesString := aNode value.
	slots := classVariablesString substrings.
	slotNodes := slots
		collect: [ :slotName | | start |
			start := aNode start + (classVariablesString findString: slotName).
			CDSharedVariableNode
				node: aNode
				name: slotName
				slotClassName: 'ClassVariableSlot'
				start: start
				stop: start + slotName size ].
	classDefinition sharedSlots: slotNodes
]

{ #category : #testing }
CDClassDefinitionParser >> isInstanceSideDefinition: aRBMessageNode [
	"Based on the Point or Point class structure, returns whether the definition is a class or instance side definition."
	^ aRBMessageNode receiver isMessage not
]

{ #category : #accessing }
CDClassDefinitionParser >> package: aNode [ 
	
	classDefinition packageNameNode: aNode astNode: aNode
]

{ #category : #'parsing main' }
CDClassDefinitionParser >> parse: aString [ 
	
	| expressionTree |
	expressionTree := RBParser parseExpression: aString.
	expressionTree doSemanticAnalysis.
	
	^ self parseRootNode: expressionTree
]

{ #category : #parsing }
CDClassDefinitionParser >> parseClassName: aNode [
	| className superclassNode |
	className := (aNode isVariable
		ifTrue: [ aNode name ]
		ifFalse: [ aNode value ]) asSymbol.
	superclassNode := self classNameNodeClass new
		originalNode: aNode;
		className: className.
	classDefinition className: className astNode: superclassNode
]

{ #category : #parsing }
CDClassDefinitionParser >> parseClassName: aNode withType: aSymbol [
	
	self parseClassName: aNode.
	classDefinition classKind: aSymbol
]

{ #category : #parsing }
CDClassDefinitionParser >> parseInstanceVariablesFromNode: aNode [
	| slots slotNodes instanceVariablesString |
	instanceVariablesString := aNode value.
	slots := instanceVariablesString substrings.
	slotNodes := slots
		collect: [ :slotName | 
			| start |
			start := aNode start
				+ (instanceVariablesString findString: slotName).
			self slotNodeClass
				node: aNode
				name: slotName
				slotClassName: #InstanceVariableSlot
				start: start
				stop: start + slotName size ].
	classDefinition slots: slotNodes
]

{ #category : #parsing }
CDClassDefinitionParser >> parsePoolDictionariesFromNode: aNode [
	| shareds sharedPoolNodes sharedPoolString |
	sharedPoolString := aNode value.
	shareds := sharedPoolString substrings.
	sharedPoolNodes := shareds
		collect: [ :slotName | 
			| start |
			start := aNode start + (sharedPoolString findString: slotName).
			self sharedPoolNodeClass
				node: aNode
				name: slotName
				start: start
				stop: start + slotName size ].
	classDefinition sharedPools: sharedPoolNodes
]

{ #category : #parsing }
CDClassDefinitionParser >> parseRootNode: expressionTree [
	self setAdequateClassDefinitionFrom: expressionTree.
	classDefinition originalNode: expressionTree.
	expressionTree acceptVisitor: self.
	"why do we need this double dispatch?"
	^ classDefinition
]

{ #category : #parsing }
CDClassDefinitionParser >> parseSelectorPart: aString withArgument: aNode [ 
	
	"We could do this with reflection, or with a dictionary and closures.
	I chose to use a series of if for readability only."
	
	(#(variableWordSubclass: ephemeronSubclass: weakSubclass: variableByteSubclass: variableSubclass: immediateSubclass: #subclass:) includes: aString)
		ifTrue: [ ^ self parseClassName: aNode withType: aString  ].
	aString =	 #instanceVariableNames:
		ifTrue: [ ^ self parseInstanceVariablesFromNode: aNode ].
	aString =	 #slots:
		ifTrue: [ ^ self parseSlotsNodesFromArrayNode: aNode ].
	aString =	 #classVariableNames:
		ifTrue: [ ^ self classVariableNames: aNode ].
	aString =	 #category:
		ifTrue: [ ^ self package: aNode ].	
	aString =	 #package:
		ifTrue: [ ^ self package: aNode ].
	aString =	 #uses:
		ifTrue: [ ^ self parseTraitDefinitionFromNode: aNode ].
	aString =	 #poolDictionaries:
		ifTrue: [ ^ self parsePoolDictionariesFromNode: aNode ].
	
	CDUnrecognizedClassPart new
		classDefinition: self; 
		unrecognizedPart: aString;
		signal
]

{ #category : #parsing }
CDClassDefinitionParser >> parseSlotNode: aRBMessageNode [
	"what ugly method! Pay attention start and stop should be tested and probably adjusted!"

	"when a slot is just 'inst'"
	aRBMessageNode isLiteralNode
		ifTrue: [ | slot |
			slot := self slotNodeClass
				node: aRBMessageNode
				name: aRBMessageNode value
				slotClassName: #InstanceVariableSlot
				initializationMessage: self slotInitializationNodeClass new
				start: aRBMessageNode start
				stop: aRBMessageNode stop.
			classDefinition addSlot: slot.
			^ self ].
	
	aRBMessageNode selector = '=>' 
		ifTrue: [  
		aRBMessageNode arguments first isMessage
		
		ifFalse: [  
			"when a slot is just 'inst' => InstanceVariableSlot."
			aRBMessageNode arguments first isGlobal
				ifTrue: [ | slot |
					slot := self slotNodeClass
						node: aRBMessageNode
						name: aRBMessageNode receiver value
						slotClassName: aRBMessageNode arguments first name
						initializationMessage: self slotInitializationNodeClass new
						start: aRBMessageNode start
						stop: aRBMessageNode stop.
						classDefinition addSlot: slot.
				^ self ].
				"we will have to do something for example if we have 
				#x => 2"
				]
			ifTrue: [  
				
				"when a slot is 'inst' => (InstanceVariableSlot default: 5)."
				| slot slotDefNode |
				slotDefNode := aRBMessageNode.
				slot := self slotNodeClass
					node: aRBMessageNode
					name: slotDefNode receiver value
					slotClassName: slotDefNode arguments first receiver name
					initializationMessage:
						(self slotInitializationNodeClass
							selectorParts: aRBMessageNode arguments first selectorParts
							argumentParts: aRBMessageNode arguments first arguments)
					start: aRBMessageNode start
					stop: aRBMessageNode stop.
				classDefinition addSlot: slot.
				^ self ]
		]
]

{ #category : #parsing }
CDClassDefinitionParser >> parseSlotsNodesFromArrayNode: aRBArrayNode [
	
	aRBArrayNode statements do: [ :slotStatement |
		self parseSlotNode: slotStatement ]
]

{ #category : #parsing }
CDClassDefinitionParser >> parseSuperclassNode: aSuperclassNode [
	| aSuperclassName superclassNode |
	aSuperclassName := (aSuperclassNode isLiteralNode
		ifTrue: [ aSuperclassNode value asString ]
		ifFalse: [ aSuperclassNode name ]) asSymbol.
	superclassNode := self classNameNodeClass new
		originalNode: aSuperclassNode;
		className: aSuperclassName.
	classDefinition
		superclassName: aSuperclassName
		astNode: superclassNode
]

{ #category : #parsing }
CDClassDefinitionParser >> parseTraitDefinitionFromNode: aNode [
	
	| traitComposition |
	traitComposition := CDTraitCompositionBuilder new buildFrom: aNode.	
	classDefinition traitDefinition: traitComposition.
]

{ #category : #internal }
CDClassDefinitionParser >> setAdequateClassDefinitionFrom: expressionTree [
	"set a class or metaclass class definition as class definition object."
	(self isInstanceSideDefinition: expressionTree)
		ifTrue: [ self beClassDefinition ]
		ifFalse: [ self beMetaclassDefinition ]
]

{ #category : #'private class factory' }
CDClassDefinitionParser >> sharedPoolNodeClass [
	^ CDSharedPoolNode
]

{ #category : #'private class factory' }
CDClassDefinitionParser >> slotInitializationNodeClass [

	^ RBSlotInitializationNode 
]

{ #category : #'private class factory' }
CDClassDefinitionParser >> slotNodeClass [
	^ CDSlotNode
]

{ #category : #parsing }
CDClassDefinitionParser >> visitMessageNode: aRBMessageNode [

	(self isInstanceSideDefinition: aRBMessageNode)
		ifTrue: [ self parseSuperclassNode: aRBMessageNode receiver ]
		ifFalse: [ self parseClassName: aRBMessageNode receiver receiver ].
	
	aRBMessageNode selectorParts
		with: aRBMessageNode arguments
		do: [ :selectorPart :argument |
			self parseSelectorPart: selectorPart withArgument: argument ]
]
