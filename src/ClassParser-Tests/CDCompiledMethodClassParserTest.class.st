Class {
	#name : 'CDCompiledMethodClassParserTest',
	#superclass : 'CDClassDefinitionParserTest',
	#category : 'ClassParser-Tests',
	#package : 'ClassParser-Tests'
}

{ #category : 'helpers' }
CDCompiledMethodClassParserTest >> classDefinitionString [
	"we just test the definition as it appears in the image"
	^ 'CompiledCode << #CompiledMethod
	layout: CompiledMethodLayout;
	package: #MyPackage'
]

{ #category : 'accessing' }
CDCompiledMethodClassParserTest >> className [
	^#CompiledMethod
]

{ #category : 'accessing' }
CDCompiledMethodClassParserTest >> superclassName [
	^#CompiledCode
]

{ #category : 'tests' }
CDCompiledMethodClassParserTest >> testBestNodeForClassVariableSelectionShouldBeClassSlotNode [
	"no Class Variables in CompileMethod"
	self assert: classDefinition sharedVariableNodes isEmpty
]

{ #category : 'tests' }
CDCompiledMethodClassParserTest >> testBestNodeForInstanceVariableSelectionShouldBeSlotNode [
	"no slots for CompileMethod"
	self assert: classDefinition slots isEmpty
]

{ #category : 'tests' }
CDCompiledMethodClassParserTest >> testBestNodeForSecondInstanceVariableSelectionShouldBeSecondSlotNode [
	"no slots for CompileMethod"
	self assert: classDefinition slots isEmpty
]

{ #category : 'tests' }
CDCompiledMethodClassParserTest >> testClassDefFromLegacyStringHasSharedSlots [
	"no Class Variables in CompileMethod"
	self assert: classDefinition sharedVariableNodes isEmpty
]

{ #category : 'tests' }
CDCompiledMethodClassParserTest >> testClassDefFromLegacyStringHasSlots [
	"no slots for CompileMethod"
	self assert: classDefinition slots isEmpty
]

{ #category : 'helpers' }
CDCompiledMethodClassParserTest >> testCompiledMethodClass [

	self assert: classDefinition layoutClass equals: CompiledMethodLayout
]

{ #category : 'tests' }
CDCompiledMethodClassParserTest >> testSlotNodesHaveParentReference [
	"no slots for CompileMethod"
	self assert: classDefinition slots isEmpty
]
