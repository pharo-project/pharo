"
I am a ridiculous alternate compiler, that replaces any literal symbol #dontReturnThis with the symbol #expectedReturn.

It's the simplest change to the compiler that I could imagine that doesn't interfere with the fact that the class-side `compilerClass` method can't be interfered with.

"
Class {
	#name : 'OCTargetCompiler',
	#superclass : 'OpalCompiler',
	#category : 'OpalCompiler-Tests-Semantic',
	#package : 'OpalCompiler-Tests',
	#tag : 'Semantic'
}

{ #category : 'accessing' }
OCTargetCompiler >> compilationContext [
	^ compilationContext ifNil: [ compilationContext := self compilationContextClass default parserClass: OCTargetCompilerParser ]
]
