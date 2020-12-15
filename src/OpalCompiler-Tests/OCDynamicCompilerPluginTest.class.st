Class {
	#name : #OCDynamicCompilerPluginTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Plugins'
}

{ #category : #tests }
OCDynamicCompilerPluginTest >> testCreateAndUseDynamicCompilerPlugin [
	| result plugin |
	plugin := OCDynamicCompilerPlugin 
					newFromTransformBlock: [ :ast | (RBParseTreeRewriter replaceLiteral: 42 with: 'meaning of life') executeTree: ast. ast ]
					andPriority: 0.
					
	"Use plugin with compiler"				
	result := Object compiler 
					addPlugin: plugin;
					evaluate: '42'.
					
	self assert: result equals: 'meaning of life'
]

{ #category : #tests }
OCDynamicCompilerPluginTest >> testCreateDynamicCompilerPluginWithDefaultPriority [

	| plugin |
	plugin := OCDynamicCompilerPlugin newFromTransformBlock: [ :ast | ast ].		
	self assert: plugin priority equals: OCDynamicCompilerPlugin defaultPriority
]
