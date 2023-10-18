Class {
	#name : 'CDCompiledBlockClassParserTest',
	#superclass : 'CDClassDefinitionParserTest',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'helpers' }
CDCompiledBlockClassParserTest >> classDefinitionString [
	"we just test the definition as it appears in the image"

	^ 'CompiledCode << #CompiledBlock
	layout: CompiledMethodLayout;
	package: #MyPackage'
]

{ #category : 'accessing' }
CDCompiledBlockClassParserTest >> className [
	^#CompiledBlock
]

{ #category : 'accessing' }
CDCompiledBlockClassParserTest >> superclassName [
	^#CompiledCode
]

{ #category : 'tests' }
CDCompiledBlockClassParserTest >> testBestNodeForClassVariableSelectionShouldBeClassSlotNode [
	"no Class Variables in CompileBlock"

	self assertEmpty: classDefinition sharedVariableNodes
]

{ #category : 'tests' }
CDCompiledBlockClassParserTest >> testBestNodeForInstanceVariableSelectionShouldBeSlotNode [
	"no slots for CompileBlock"
	self assert: classDefinition slots isEmpty
]

{ #category : 'tests' }
CDCompiledBlockClassParserTest >> testBestNodeForSecondInstanceVariableSelectionShouldBeSecondSlotNode [
	"no slots for CompileBlock"
	self assert: classDefinition slots isEmpty
]

{ #category : 'tests' }
CDCompiledBlockClassParserTest >> testClassDefFromLegacyStringHasSharedSlots [
	"no Class Variables in CompileBlock"
	self assert: classDefinition sharedVariableNodes isEmpty
]

{ #category : 'tests' }
CDCompiledBlockClassParserTest >> testClassDefFromLegacyStringHasSlots [
	"no slots for CompileBlock"
	self assert: classDefinition slots isEmpty
]

{ #category : 'helpers' }
CDCompiledBlockClassParserTest >> testCompiledMethodClass [

	self assert: classDefinition layoutClass equals: CompiledMethodLayout
]

{ #category : 'tests' }
CDCompiledBlockClassParserTest >> testSlotNodesHaveParentReference [
	"no slots for CompileBlock"
	self assert: classDefinition slots isEmpty
]
