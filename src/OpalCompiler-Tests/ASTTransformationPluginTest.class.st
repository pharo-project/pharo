Class {
	#name : #ASTTransformationPluginTest,
	#superclass : #TestCase,
	#category : #OpalCompiler-Tests-Plugins
}

{ #category : #tests }
ASTTransformationPluginTest >> testClassWithPluginEnabled [
	self assert: ASTTransformExamplePluginActive new example42 = 'meaning of life'
]

{ #category : #tests }
ASTTransformationPluginTest >> testTransform [
		| ast |
		ast := (OCOpalExamples>>#exampleReturn42) ast copy.
		self assert: ast body statements first value value =  42.
		ast := ASTPluginMeaningOfLife transform: ast.
		self assert: ast body statements first value value =  'meaning of life'.
]
