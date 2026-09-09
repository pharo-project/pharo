Class {
	#name : 'OCSuccessCompilationResult',
	#superclass : 'Object',
	#instVars : [
		'compiledMethod'
	],
	#category : 'OpalCompiler-Core-FrontEnd',
	#package : 'OpalCompiler-Core',
	#tag : 'FrontEnd'
}

{ #category : 'accessing' }
OCSuccessCompilationResult >> compiledMethod [

	^ compiledMethod
]

{ #category : 'accessing' }
OCSuccessCompilationResult >> compiledMethod: anObject [

	compiledMethod := anObject
]

{ #category : 'testing' }
OCSuccessCompilationResult >> isError [

	^ false
]
