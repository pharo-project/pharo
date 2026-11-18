Class {
	#name : 'OCCompilationError',
	#superclass : 'Error',
	#instVars : [
		'compilationResult'
	],
	#category : 'OpalCompiler-Core-Exceptions',
	#package : 'OpalCompiler-Core',
	#tag : 'Exceptions'
}

{ #category : 'accessing' }
OCCompilationError >> compilationResult [

	^ compilationResult
]

{ #category : 'accessing' }
OCCompilationError >> compilationResult: anObject [

	compilationResult := anObject
]

{ #category : 'initialization' }
OCCompilationError >> initialize [

	super initialize.
	messageText := self messageText
]

{ #category : 'accessing' }
OCCompilationError >> messageText [
	"Name the method that failed: a load compiles many methods, and the notices name only the selector"

	| ast notices res |
	ast := compilationResult ifNotNil: [ compilationResult ast ].
	ast ifNil: [ ^ super messageText ].

	notices := ast allNotices.
	notices ifEmpty: [ ^ super messageText ].

	res := (notices collect: [ :each | '- ' , each asString ]) joinUsing: OSPlatform current lineEnding.
	^ 'Compilation of ' , self methodName , ' failed with following errors:' , OSPlatform current lineEnding , res
]

{ #category : 'accessing' }
OCCompilationError >> methodName [
	"The method being compiled, as Class>>selector. A source that does not parse has no class yet"

	| ast |
	ast := compilationResult ast methodNode.
	^ ast methodClass
		  ifNil: [ ast selector asString ]
		  ifNotNil: [ :class | class name , '>>' , ast selector ]
]
