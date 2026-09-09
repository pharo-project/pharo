Class {
	#name : 'OpalEvaluator',
	#superclass : 'Object',
	#instVars : [
		'failBlock',
		'permitFaultyEvaluation',
		'compiler',
		'logged',
		'bindings',
		'context',
		'receiver'
	],
	#category : 'OpalCompiler-Core-FrontEnd',
	#package : 'OpalCompiler-Core',
	#tag : 'FrontEnd'
}

{ #category : 'adding' }
OpalEvaluator >> addParsePlugin: anOCDynamicASTCompilerPlugin [

	compiler addParsePlugin: anOCDynamicASTCompilerPlugin
]

{ #category : 'adding' }
OpalEvaluator >> addPlugin: aPlugin [

	compiler addPlugin: aPlugin
]

{ #category : 'accessing' }
OpalEvaluator >> bindings: aCollection [

	bindings := aCollection
]

{ #category : 'adding' }
OpalEvaluator >> context: aContext [

	context := aContext.
	
	"Evaluation happens on the dynamic context if any.
	This means that `self` is bound to the context receiver"
	self receiver: aContext receiver
]

{ #category : 'accessing' }
OpalEvaluator >> evaluate: aString [

	| compilationResult |
	compiler source: aString.
	compiler permitFaulty: (permitFaultyEvaluation ifNil: [ false ]).
	compiler permitUndeclared: compiler permitFaulty.
	compiler logged: logged.
	compiler bindings: bindings.
	
	receiver ifNotNil: [ 
		compiler receiver: receiver ].
	context ifNotNil: [
		compiler context: context ].
	
	compilationResult := compiler doCompile.

	compilationResult isError ifTrue: [
		(compilationResult ast allNotices select: [ :e | e isErrorOn: compiler ]) sorted do: [ :n |
			failBlock ifNotNil: [ ^ failBlock cull: n ].
			n signalError ].
		^ nil
	].

	^ receiver withArgs: #(  ) executeMethod: compilationResult compiledMethod
]

{ #category : 'accessing' }
OpalEvaluator >> failBlock: aFullBlockClosure [ 
	
	failBlock := aFullBlockClosure
]

{ #category : 'initialization' }
OpalEvaluator >> initialize [

	super initialize.
	logged := false.
	bindings := #().
	compiler := OpalCompiler new.
	compiler isScripting: true
]

{ #category : 'accessing' }
OpalEvaluator >> logged: aBoolean [

	logged := aBoolean
]

{ #category : 'public access' }
OpalEvaluator >> permitFaulty: aBoolean [ 
	
	permitFaultyEvaluation := aBoolean
]

{ #category : 'adding' }
OpalEvaluator >> receiver: aReceiver [

	receiver := aReceiver
]
