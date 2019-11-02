Extension { #name : #SmalltalkImage }

{ #category : #'*OpalCompiler-Core' }
SmalltalkImage >> compilerClass [
	^CompilerClass ifNil: [CompilerClass := OpalCompiler].
]
