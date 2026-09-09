Class {
	#name : 'OCErrorCompilationResult',
	#superclass : 'Object',
	#instVars : [
		'ast'
	],
	#category : 'OpalCompiler-Core-FrontEnd',
	#package : 'OpalCompiler-Core',
	#tag : 'FrontEnd'
}

{ #category : 'accessing' }
OCErrorCompilationResult >> ast [

	^ ast
]

{ #category : 'accessing' }
OCErrorCompilationResult >> ast: anObject [

	ast := anObject
]

{ #category : 'testing' }
OCErrorCompilationResult >> isError [

	^ true
]

{ #category : 'testing' }
OCErrorCompilationResult >> signalError [

	^ OCCompilationError new
		compilationResult: self;
		signal
]
