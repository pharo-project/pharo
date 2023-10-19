Class {
	#name : 'CDClassDefinitionParserTest',
	#superclass : 'CDBehaviorParserTest',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'testing' }
CDClassDefinitionParserTest class >> isAbstract [
	^ self == CDClassDefinitionParserTest
]

{ #category : 'helpers' }
CDClassDefinitionParserTest >> firstClassVariableIndex [

	^ self classDefinitionString findString: self firstClassVariableName
]

{ #category : 'tests' }
CDClassDefinitionParserTest >> packageName [
	^  #MyPackage
]

{ #category : 'tests' }
CDClassDefinitionParserTest >> testBestNodeForClassVariableSelectionShouldBeClassSlotNode [

	| selectedNode selection |
	selection := self selectionOf: self firstClassVariableName.

	selectedNode := classDefinition bestNodeFor: selection.
	self assert: selectedNode equals: classDefinition sharedVariables first
]

{ #category : 'tests' }
CDClassDefinitionParserTest >> testBestNodeForSuperclassSelectionShouldBeSuperclassNameNode [

	| selectedNode selection |
	selection := self selectionOf: self superclassName.

	selectedNode := classDefinition bestNodeFor: selection.
	self assert: selectedNode equals: classDefinition superclassNameNode
]

{ #category : 'tests' }
CDClassDefinitionParserTest >> testClassDefFromLegacyStringHasClassNameNode [

	self assert: classDefinition classNameNode value equals: self className
]

{ #category : 'tests' }
CDClassDefinitionParserTest >> testClassDefFromLegacyStringHasPackageName [

	self assert: classDefinition packageName equals: self packageName
]

{ #category : 'tests' }
CDClassDefinitionParserTest >> testClassDefFromLegacyStringHasPackageNameNode [

	self assert: classDefinition packageNameNode value equals: self packageName
]

{ #category : 'tests' }
CDClassDefinitionParserTest >> testClassDefFromLegacyStringHasSharedSlots [

	self assert: classDefinition sharedVariables first name equals: self firstClassVariableName.
	self assert: classDefinition sharedVariables second name equals: self secondClassVariableName.

	self assert: classDefinition sharedVariables second isLiteralVariable
]

{ #category : 'tests' }
CDClassDefinitionParserTest >> testClassDefFromLegacyStringHasSuperclassName [

	self assert: classDefinition superclassName equals: self superclassName
]

{ #category : 'tests' }
CDClassDefinitionParserTest >> testClassDefFromLegacyStringHasSuperclassNameNode [

	self assert: classDefinition superclassNameNode name equals: self superclassName
]
