Class {
	#name : #OCAnnotationTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Plugins'
}

{ #category : #tests }
OCAnnotationTest >> testAnnotationAST [

	| plugin result |
	plugin := OCDynamicASTCompilerPlugin
		          newFromTransformBlock: [ :ast |
			          (RBParseTreeRewriter new
				           replace: '@ meaning: `@arg'
				           withValueFrom: [ :node |
					           RBLiteralNode value:
						           ((node arguments first isLiteralNode and: [
							             node arguments first value = 42 ])
							            ifTrue: [ 'meaning of life' ]
							            ifFalse: [ 'no meaning' ]) ]) executeTree: ast.
			          ast ]
		          andPriority: 0.

	"Use plugin with compiler"
	result := Object compiler
		          addPlugin: plugin;
		          evaluate:
			          '{@meaning: 42. @meaning: 12. @meaning: self error. 42}'.
	self
		assert: result
		equals: { 'meaning of life'. 'no meaning'. 'no meaning'. 42 }
]

{ #category : #tests }
OCAnnotationTest >> testAnnotationIR [

	| plugin result |
	plugin := OCDynamicASTCompilerPlugin
		          newFromTransformBlock: [ :ast | "In fact, there is no transformation. Just an addition to the annotation mark"
			          (RBParseTreeSearcher new
				           matches: '@ meaning: `@arg'
				           do: [ :node :ans |
					           node receiver emitValueBlock: [ :methodBuilder |
						           methodBuilder pushLiteral:
							           ((node arguments first isLiteralNode and: [
								             node arguments first value = 42 ])
								            ifTrue: [ 'meaning of life' ]
								            ifFalse: [ 'no meaning' ]) ] ]) executeTree: ast.
			          ast ]
		          andPriority: 0.

	"Use plugin with compiler"
	result := Object compiler
		          addPlugin: plugin;
		          evaluate:
			          '{@meaning: 42. @meaning: 12. @meaning: self error. 42}'.
	self
		assert: result
		equals: { 'meaning of life'. 'no meaning'. 'no meaning'. 42 }
]
