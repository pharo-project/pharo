"
I'm a common superclass for class definition parsers. 

"
Class {
	#name : #CDAbstractClassDefinitionParser,
	#superclass : #Object,
	#instVars : [
		'classDefinition'
	],
	#category : #'ClassParser-Parser'
}

{ #category : #parsing }
CDAbstractClassDefinitionParser class >> fromASTNode: aNode [

	^ self new parseRootNode: aNode
]

{ #category : #testing }
CDAbstractClassDefinitionParser class >> isAbstract [

	^ self == CDAbstractClassDefinitionParser
]

{ #category : #'instance creation' }
CDAbstractClassDefinitionParser class >> parse: aString [

	^ self new parse: aString
]

{ #category : #internal }
CDAbstractClassDefinitionParser >> beClassDefinition [

	classDefinition := CDClassDefinitionNode new
]

{ #category : #internal }
CDAbstractClassDefinitionParser >> beClassSideTraitDefinition [

	classDefinition := CDClassSideTraitDefinitionNode new
]

{ #category : #internal }
CDAbstractClassDefinitionParser >> beMetaclassDefinition [

	classDefinition := CDMetaclassDefinitionNode new
]

{ #category : #internal }
CDAbstractClassDefinitionParser >> beTraitDefinition [

	classDefinition := CDTraitDefinitionNode new
]

{ #category : #'private - class factory' }
CDAbstractClassDefinitionParser >> classNameNodeClass [
	^ CDClassNameNode
]

{ #category : #parsing }
CDAbstractClassDefinitionParser >> handleClassName: aNode [
	| className classNode |
	className := (aNode isVariable
		ifTrue: [ aNode name ]
		ifFalse: [ aNode value ]) asSymbol.
	classNode := self classNameNodeClass new
		originalNode: aNode;
		className: className.
	classDefinition className: className astNode: classNode
]

{ #category : #parsing }
CDAbstractClassDefinitionParser >> handleInstanceVariablesFromNode: aNode [
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

{ #category : #parsing }
CDAbstractClassDefinitionParser >> handleMetaclassName: aNode [
	"we are in situation ClassX class << ClassZ class
				slots: {xxxx} so grab the class"

	aNode handleMetaclassName: self

	"| className classNode node |
	node := aNode receiver arguments first.
	className := node receiver binding value class name.
	classNode := self classNameNodeClass new
		originalNode: node;
		className: className.
	classDefinition className: node astNode: classNode
	"
]

{ #category : #parsing }
CDAbstractClassDefinitionParser >> handlePackage: aNode [

	classDefinition packageNameNode: aNode astNode: aNode
]

{ #category : #parsing }
CDAbstractClassDefinitionParser >> handleSharedPoolsFromNode: aNode [
	^ self subclassResponsibility
]

{ #category : #parsing }
CDAbstractClassDefinitionParser >> handleSharedVariableNames: aNode [

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

{ #category : #parsing }
CDAbstractClassDefinitionParser >> handleSuperclassNode: aNode [
	^ self subclassResponsibility
]

{ #category : #parsing }
CDAbstractClassDefinitionParser >> handleTraitUsesFromNode: aNode [

	| traitComposition |
	aNode isDynamicArray and: [aNode allStatements ifEmpty: [ ^ self ]].
 	traitComposition := CDTraitCompositionBuilder new buildFrom: aNode.
	classDefinition traitDefinition: traitComposition
]

{ #category : #parsing }
CDAbstractClassDefinitionParser >> parse: aString [
	| expressionTree |
	expressionTree := self parserClass parseExpression: aString.
	[expressionTree doSemanticAnalysis]
		on: OCUndeclaredVariableWarning
		do: [ :ex | ex resume: ex declareUndefined ].
	^ self parseRootNode: expressionTree
]

{ #category : #parsing }
CDAbstractClassDefinitionParser >> parseRootNode: aNode [
	^ self subclassResponsibility
]

{ #category : #parsing }
CDAbstractClassDefinitionParser >> parseSelectorPart: aString withArgument: aNode [
	^ self subclassResponsibility
]

{ #category : #'private - class factory' }
CDAbstractClassDefinitionParser >> parserClass [
	^ RBParser
]

{ #category : #'private - class factory' }
CDAbstractClassDefinitionParser >> sharedPoolNodeClass [
	^ CDSharedPoolNode
]

{ #category : #'private - class factory' }
CDAbstractClassDefinitionParser >> slotNodeClass [
	^ CDSlotNode
]

{ #category : #parsing }
CDAbstractClassDefinitionParser >> visitMessageNode: aNode [
	^ self subclassResponsibility
]
