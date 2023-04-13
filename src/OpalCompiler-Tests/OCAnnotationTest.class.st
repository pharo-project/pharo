Class {
	#name : #OCAnnotationTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Plugins'
}

{ #category : #tests }
OCAnnotationTest >> testAnnotationAST [
	"This test uses an AST plugin to detect and transform a custom annotation `@meaning:`
	(using `OCDynamicASTCompilerPlugin` and `RBParseTreeRewriter`).
	
	The annotation (the full message node) is transformed into a simple literal node."

	| plugin result |
	plugin := OCDynamicASTCompilerPlugin
		          newFromTransformBlock: [ :ast |
			          (RBParseTreeRewriter new
				           replace: '@meaning: `@arg'
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
		          addParsePlugin: plugin;
		          evaluate:
			          '{@meaning: 42. @meaning: 12. @meaning: self error. 42}'.
	self
		assert: result
		equals: { 'meaning of life'. 'no meaning'. 'no meaning'. 42 }
]

{ #category : #tests }
OCAnnotationTest >> testAnnotationIR [
	"This test uses an AST plugin to detect a custom annotation `@meaning:`
	(using `OCDynamicASTCompilerPlugin` and `RBParseTreeSearcher`)
	and attach a custom `emitValueBlock` to it.
	It does not transform the AST.

	At translation-time (AST->IR), the annotation (the full message node) is
	compiled with the help of the attached `emitValueBlock`."

	| plugin result |
	plugin := OCDynamicASTCompilerPlugin
		          newFromTransformBlock: [ :ast | "In fact, there is no transformation. Just an addition to the annotation mark"
			          (RBParseTreeSearcher new
				           matches: '@meaning: `@arg'
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
		          addParsePlugin: plugin;
		          evaluate:
			          '{@meaning: 42. @meaning: 12. @meaning: self error. 42}'.
	self
		assert: result
		equals: { 'meaning of life'. 'no meaning'. 'no meaning'. 42 }
]
