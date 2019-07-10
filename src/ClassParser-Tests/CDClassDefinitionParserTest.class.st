Class {
	#name : #CDClassDefinitionParserTest,
	#superclass : #CDBehaviorParserTest,
	#category : #'ClassParser-Tests'
}

{ #category : #testing }
CDClassDefinitionParserTest class >> isAbstract [
	^ self == CDClassDefinitionParserTest
]

{ #category : #helpers }
CDClassDefinitionParserTest >> firstClassVariableIndex [

	^ self classDefinitionString findString: self firstClassVariableName
]

{ #category : #helpers }
CDClassDefinitionParserTest >> firstClassVariableName [

	^ 'firstClass'
]

{ #category : #tests }
CDClassDefinitionParserTest >> packageName [
	^  #MyPackage
]

{ #category : #helpers }
CDClassDefinitionParserTest >> secondClassVariableName [

	^ 'secondClass'
]

{ #category : #helpers }
CDClassDefinitionParserTest >> superclassName [

	^ 'TheSuperClass'
]

{ #category : #tests }
CDClassDefinitionParserTest >> testBestNodeForClassVariableSelectionShouldBeClassSlotNode [

	| selectedNode selection |	
	selection := self selectionOf: self firstClassVariableName.
	
	selectedNode := classDefinition bestNodeFor: selection.
	self assert: selectedNode equals: classDefinition sharedSlots first
]

{ #category : #tests }
CDClassDefinitionParserTest >> testBestNodeForSuperclassSelectionShouldBeSuperclassNameNode [

	| selectedNode selection |
	selection := self selectionOf: self superclassName.

	selectedNode := classDefinition bestNodeFor: selection.
	self assert: selectedNode equals: classDefinition superclassNameNode
]

{ #category : #tests }
CDClassDefinitionParserTest >> testClassDefFromLegacyStringHasClassNameNode [

	self assert: classDefinition classNameNode value equals: self className.
]

{ #category : #tests }
CDClassDefinitionParserTest >> testClassDefFromLegacyStringHasClassSlots [

	self assert: classDefinition sharedSlotNodes first name equals: self firstClassVariableName.
	self assert: classDefinition sharedSlotNodes second name equals: self secondClassVariableName.
]

{ #category : #tests }
CDClassDefinitionParserTest >> testClassDefFromLegacyStringHasPackageName [

	self assert: classDefinition packageName equals: self packageName
]

{ #category : #tests }
CDClassDefinitionParserTest >> testClassDefFromLegacyStringHasPackageNameNode [

	self assert: classDefinition packageNameNode value equals: self packageName
]

{ #category : #tests }
CDClassDefinitionParserTest >> testClassDefFromLegacyStringHasSharedSlots [

	self assert: classDefinition sharedSlots first name equals: self firstClassVariableName.
	self assert: classDefinition sharedSlots second name equals: self secondClassVariableName.
]

{ #category : #tests }
CDClassDefinitionParserTest >> testClassDefFromLegacyStringHasSuperclassName [

	self assert: classDefinition superclassName equals: self superclassName.
]

{ #category : #tests }
CDClassDefinitionParserTest >> testClassDefFromLegacyStringHasSuperclassNameNode [

	self assert: classDefinition superclassNameNode name equals: self superclassName.
]
