Class {
	#name : #OCCompilerDynamicASTPluginTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Plugins'
}

{ #category : #tests }
OCCompilerDynamicASTPluginTest >> testCreatingAndUsingDynamicCompilerPlugin [
	| result |
	result :=
	Object compiler
		addPlugin: 
			(OCCompilerDynamicASTPlugin 
				newFromTransformBlock: [ :ast | (RBParseTreeRewriter replaceLiteral: 42 with: 'meaning of life') executeTree: ast. ast. ]
				andPriority: 0
			);
		evaluate: '42'.
	self assert: result equals: 'meaning of life'.
]
