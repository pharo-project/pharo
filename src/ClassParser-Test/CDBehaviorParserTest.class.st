Class {
	#name : #CDBehaviorParserTest,
	#superclass : #TestCase,
	#instVars : [
		'classDefinition'
	],
	#category : #'ClassParser-Test'
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
CDBehaviorParserTest >> firstInstanceVariableIndex [

	^ self classDefinitionString findString: self firstInstanceVariableName
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
CDBehaviorParserTest >> secondClassVariableIndex [

	^ self classDefinitionString findString: self secondClassVariableName
]

{ #category : #helpers }
CDBehaviorParserTest >> secondInstanceVariableIndex [

	^ self classDefinitionString findString: self secondInstanceVariableName
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

{ #category : #tests }
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
