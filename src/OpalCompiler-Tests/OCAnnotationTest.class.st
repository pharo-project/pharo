Class {
	#name : 'OCAnnotationTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests-Plugins',
	#package : 'OpalCompiler-Tests',
	#tag : 'Plugins'
}

{ #category : 'tests' }
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
					           ASTLiteralNode value:
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

{ #category : 'tests' }
OCAnnotationTest >> testAnnotationBinding [
	"A more advanced usage.
	The `@binding:` example annotation takes a variable and push its binding instead of reading it.
	This example takes care of errors at parse-time, so the user can statically be informed of missuses of the annotation.
	
	Note: the example nests of lot of concerns into a single statement.
	Real uses in production will likely involve a better design with more classes and methods."

	| plugin result |
	plugin := OCDynamicASTCompilerPlugin
		          newFromTransformBlock: [ :ast | "In fact, there is no transformation"
			          (RBParseTreeSearcher new
				           matches: '@binding: `@arg'
				           do: [ :node :ans | "Static syntax check"
					           node arguments first isVariable
						           ifFalse: [ "Not a variable, add a syntax error"
							           node arguments first addError: 'Variable expected'.
							           "Also add a compile-time error for faulty modes"
							           node receiver emitValueBlock: [ :methodBuilder |
								           methodBuilder
									           pushLiteralVariable: RuntimeSyntaxError binding;
									           pushLiteral: 'Variable expected';
									           send: #signal: ] ]
						           ifTrue: [ "It's a real variable, to use the binding.
						              Note: currenlty variables are not bound yet, but they will be when the block is evaluated"
							           node receiver emitValueBlock: [ :methodBuilder |
								           methodBuilder pushLiteral:
									           node arguments first variable ] ] ]) executeTree:
				          ast.
			          ast ]
		          andPriority: 0.

	"Use plugin with compiler"
	result := Object compiler
		          addParsePlugin: plugin;
		          evaluate: '@binding: Object'.
	self assert: result class equals: GlobalVariable.
	self assert: result key equals: #Object.
	self assert: result value equals: Object.

	result := Object compiler
		          addParsePlugin: plugin;
		          evaluate: '@binding: self'.
	self assert: result class equals: SelfVariable.

	result := Object compiler
		          addParsePlugin: plugin;
		          failBlock: [ :n |
			          self assert: n messageText equals: 'Variable expected'.
			          #failed ];
		          evaluate: '@binding: 42'.
	self assert: result equals: #failed.

	self
		should: [
			Object compiler
				addParsePlugin: plugin;
				permitFaulty: true;
				evaluate: '@binding: 42' ]
		raise: RuntimeSyntaxError
]

{ #category : 'tests' }
OCAnnotationTest >> testAnnotationConstexprAST [
	"This advanced example shows a `@constexpr:` annotation that evaluates its argument at compile-time.
	You might think about `##()` in some Smalltalk dialects for instance.
	
	Here the implementation use an AST approach that transform the whole annotation into a literal node"

	| plugin result |
	plugin := OCDynamicASTCompilerPlugin
		          newFromTransformBlock: [ :ast |
			          (RBParseTreeRewriter new
				           replace: '@constexpr: `@arg'
				           withValueFrom: [ :node |
					           | value |
					           "Evaluate the argument, self is bound to the class"
					           value := node arguments first evaluateForReceiver:
						                    node methodNode methodClass instanceSide.
					           ASTLiteralNode value: value ]) executeTree: ast.
			          ast ]
		          andPriority: 0.

	result := Object compiler
		          addParsePlugin: plugin;
		          compile:
			          'foo ^ @constexpr: ''The answer is '' , (19+23) asString'.
	self assert: (result literals includes: 'The answer is 42').
	self assert: (nil executeMethod: result) equals: 'The answer is 42'.

	result := Float compiler
		          addParsePlugin: plugin;
		          compile: 'foo ^ @constexpr: self pi'.
	self assert: (result literals includes: Float pi).
	self assert: (nil executeMethod: result) equals: Float pi
]

{ #category : 'tests' }
OCAnnotationTest >> testAnnotationConstexprIR [
	"This advanced example shows a `@constexpr:` annotation that evaluates its argument at compile-time.
	You might think about `##()` in some Smalltalk dialects for instance.
	
	Here the implementation use an IR approach and store the value directly as a literal of the method"

	| plugin result |
	plugin := OCDynamicASTCompilerPlugin
		          newFromTransformBlock: [ :ast | "In fact, there is no transformation. Just an addition to the annotation mark"
			          (RBParseTreeSearcher new
				           matches: '@constexpr: `@arg'
				           do: [ :node :ans |
					           | value |
					           "Evaluate the argument, self is bound to the class"
					           value := node arguments first evaluateForReceiver:
						                    node methodNode methodClass instanceSide.
					           "IR is just to store the value in literals and retrieve it at runtime"
					           node receiver emitValueBlock: [ :methodBuilder |
						           methodBuilder pushLiteral: value ] ]) executeTree:
				          ast.
			          ast ]
		          andPriority: 0.

	result := Object compiler
		          addParsePlugin: plugin;
		          compile:
			          'foo ^ @constexpr: ''The answer is '' , (19+23) asString'.
	self assert: (result literals includes: 'The answer is 42').
	self assert: (nil executeMethod: result) equals: 'The answer is 42'.

	result := Float compiler
		          addParsePlugin: plugin;
		          compile: 'foo ^ @constexpr: self pi'.
	self assert: (result literals includes: Float pi).
	self assert: (nil executeMethod: result) equals: Float pi
]

{ #category : 'tests' }
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
