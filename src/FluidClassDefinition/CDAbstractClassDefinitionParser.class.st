"
I'm a common superclass for class definition parsers. 

"
Class {
	#name : #CDAbstractClassDefinitionParser,
	#superclass : #Object,
	#instVars : [
		'classDefinition',
		'subclass'
	],
	#category : #'FluidClassDefinition-New Nodes'
}

{ #category : #'instance creation' }
CDAbstractClassDefinitionParser class >> parse: aString [ 
	
	^ self new parse: aString
]

{ #category : #internal }
CDAbstractClassDefinitionParser >> beClassDefinition [
		
	classDefinition := CDClassDefinitionNode new
]

{ #category : #internal }
CDAbstractClassDefinitionParser >> beMetaclassDefinition [
		
	classDefinition := CDMetaclassDefinitionNode new
]

{ #category : #'private class factory' }
CDAbstractClassDefinitionParser >> classNameNodeClass [
	^ CDClassNameNode
]

{ #category : #testing }
CDAbstractClassDefinitionParser >> isInstanceSideDefinition: aRBMessageNode [
	"Based on the Point or Point class structure, returns whether the definition is a class or instance side definition."
	^ self subclassResponsibility 
]

{ #category : #parsing }
CDAbstractClassDefinitionParser >> parse: aString [
	| expressionTree |
	expressionTree := self parserClass parseExpression: aString.
	expressionTree doSemanticAnalysis.
	^ self parseRootNode: expressionTree
]

{ #category : #parsing }
CDAbstractClassDefinitionParser >> parseRootNode: expressionTree [
	self setAdequateClassDefinitionFrom: expressionTree.
	classDefinition originalNode: expressionTree.
	expressionTree acceptVisitor: self.
	^ classDefinition
]

{ #category : #hooks }
CDAbstractClassDefinitionParser >> parserClass [
	^ RBParser
]

{ #category : #internal }
CDAbstractClassDefinitionParser >> setAdequateClassDefinitionFrom: expressionTree [
	"set a class or metaclass class definition as class definition object."
	(self isInstanceSideDefinition: expressionTree)
		ifTrue: [ self beClassDefinition ]
		ifFalse: [ self beMetaclassDefinition ]
]

{ #category : #'private class factory' }
CDAbstractClassDefinitionParser >> sharedPoolNodeClass [
	^ CDSharedPoolNode
]

{ #category : #'private class factory' }
CDAbstractClassDefinitionParser >> slotInitializationNodeClass [

	^ RBSlotInitializationNode 
]

{ #category : #'private class factory' }
CDAbstractClassDefinitionParser >> slotNodeClass [
	^ CDSlotNode
]
