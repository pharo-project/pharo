Class {
	#name : #ASTTransformationPluginTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Plugins'
}

{ #category : #tests }
ASTTransformationPluginTest >> testClassWithPluginEnabled [
	self assert: ASTTransformExamplePluginActive new example42 equals: 'meaning of life'
]

{ #category : #tests }
ASTTransformationPluginTest >> testTransform [
	| ast |
	ast := (OCOpalExamples >> #exampleReturn42) ast copy.
	self assert: ast body statements first value value equals: 42.
	ast := ASTPluginMeaningOfLife transform: ast.
	self assert: ast body statements first value value equals: 'meaning of life'
]
