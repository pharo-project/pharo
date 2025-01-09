Class {
	#name : 'OCStaticASTCompilerPluginTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests-Plugins',
	#package : 'OpalCompiler-Tests',
	#tag : 'Plugins'
}

{ #category : 'tests' }
OCStaticASTCompilerPluginTest >> testClassWithPluginEnabled [

 	"It is possible that the method was loaded before the plugin was active. We recompile to be sure the plugin was active."
 	OCTransformExamplePluginActive recompile.
 	self assert: OCTransformExamplePluginActive new example42 equals: 'meaning of life'
]

{ #category : 'tests' }
OCStaticASTCompilerPluginTest >> testCreateAndUseStaticCompilerPlugin [

	| result plugin |
	plugin := OCPluginMeaningOfLife new.

	"Use plugin with compiler"
	result := Object compiler
		          addPlugin: plugin;
		          evaluate: '42'.

	self assert: result equals: 'meaning of life'
]

{ #category : 'tests' }
OCStaticASTCompilerPluginTest >> testIsAbstract [

	self assert: OCStaticASTCompilerPlugin isAbstract
]

{ #category : 'tests' }
OCStaticASTCompilerPluginTest >> testTransform [

	| ast |
	ast := (OCOpalExamples >> #exampleReturn42) ast copy.
	self assert: ast body statements first value value equals: 42.
	ast := OCPluginMeaningOfLife transform: ast.
	self
		assert: ast body statements first value value
		equals: 'meaning of life'
]
