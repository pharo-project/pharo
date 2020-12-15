Class {
	#name : #OCStaticCompilerPluginTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Plugins'
}

{ #category : #tests }
OCStaticCompilerPluginTest >> testClassWithPluginEnabled [
	self assert: ASTTransformExamplePluginActive new example42 equals: 'meaning of life'
]

{ #category : #tests }
OCStaticCompilerPluginTest >> testCreateAndUseStaticCompilerPlugin [
	| result plugin |
	plugin := ASTPluginMeaningOfLife new.
					
	"Use plugin with compiler"				
	result := Object compiler 
					addPlugin: plugin;
					evaluate: '42'.
					
	self assert: result equals: 'meaning of life'
]

{ #category : #tests }
OCStaticCompilerPluginTest >> testIsAbstract [ 

	self assert: OCStaticCompilerPlugin isAbstract 
]

{ #category : #tests }
OCStaticCompilerPluginTest >> testTransform [
	| ast |
	ast := (OCOpalExamples >> #exampleReturn42) ast copy.
	self assert: ast body statements first value value equals: 42.
	ast := ASTPluginMeaningOfLife transform: ast.
	self assert: ast body statements first value value equals: 'meaning of life'
]
