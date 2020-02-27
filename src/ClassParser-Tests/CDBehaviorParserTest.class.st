Class {
	#name : #CDBehaviorParserTest,
	#superclass : #TestCase,
	#instVars : [
		'classDefinition'
	],
	#category : #'ClassParser-Tests'
}

{ #category : #testing }
CDBehaviorParserTest class >> isAbstract [
	^ self == CDBehaviorParserTest
]

{ #category : #helpers }
CDBehaviorParserTest >> classDefinitionString [

	self subclassResponsibility
]

{ #category : #helpers }
CDBehaviorParserTest >> className [

	^ 'TheClass'
]

{ #category : #helpers }
CDBehaviorParserTest >> firstInstanceVariableName [

	^ 'first'
]

{ #category : #helpers }
CDBehaviorParserTest >> nodeFor: aString [ 
	
	^ classDefinition bestNodeFor: (self selectionOf: aString)
]

{ #category : #helpers }
CDBehaviorParserTest >> secondInstanceVariableName [

	^ 'second'
]

{ #category : #helpers }
CDBehaviorParserTest >> selectionOf: aText [

	| start |
	start := self classDefinitionString findString: aText.
	^ start to: start
]

{ #category : #running }
CDBehaviorParserTest >> setUp [

	super setUp.
	classDefinition := CDClassDefinitionParser parse: self classDefinitionString.
]

{ #category : #tests }
CDBehaviorParserTest >> testBestNodeForClassNameSelectionShouldBeClassNameNode [

	| selectedNode selection |
	selection := self selectionOf: self className.
	selectedNode := classDefinition bestNodeFor: selection.
	self assert: selectedNode equals: classDefinition classNameNode
]

{ #category : #tests }
CDBehaviorParserTest >> testBestNodeForInstanceVariableSelectionShouldBeSlotNode [

	| selectedNode selection |	
	selection := self selectionOf: self firstInstanceVariableName.
	
	selectedNode := classDefinition bestNodeFor: selection.
	self assert: selectedNode equals: classDefinition slotNodes first
]

{ #category : #tests }
CDBehaviorParserTest >> testBestNodeForSecondInstanceVariableSelectionShouldBeSecondSlotNode [

	| selectedNode selection |
	selection := self selectionOf: self secondInstanceVariableName.
	
	selectedNode := classDefinition bestNodeFor: selection.
	self assert: selectedNode equals: classDefinition slotNodes second
]

{ #category : #tests }
CDBehaviorParserTest >> testClassDefFromLegacyStringHasClassName [

	self assert: classDefinition className equals: self className
]

{ #category : #tests }
CDBehaviorParserTest >> testClassDefFromLegacyStringHasSlots [

	self assert: classDefinition slotNodes first name equals: self firstInstanceVariableName.
	self assert: classDefinition slotNodes second name equals: self secondInstanceVariableName.
]

{ #category : #tests }
CDBehaviorParserTest >> testClassNameNodeHaveParentReference [

	| nameNode |
	nameNode := classDefinition classNameNode.
	
	self assert: nameNode parent identicalTo: classDefinition.
	self assert: nameNode classDefinitionNode identicalTo: classDefinition.
	self assert: (classDefinition children includes: nameNode)
]

{ #category : #tests }
CDBehaviorParserTest >> testSlotNodesHaveParentReference [

	| slotNode |
	slotNode := classDefinition slotNodes first.
	
	self assert: slotNode parent identicalTo: classDefinition.
	self assert: slotNode classDefinitionNode identicalTo: classDefinition.
	self assert: (classDefinition children includes: slotNode)
]
