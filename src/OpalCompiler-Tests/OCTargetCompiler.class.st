Class {
	#name : #OCTargetCompiler,
	#superclass : #OpalCompiler,
	#category : #'OpalCompiler-Tests-Semantic'
}

{ #category : #accessing }
OCTargetCompiler >> compilationContext [
	^ compilationContext ifNil: [ compilationContext := self compilationContextClass default parserClass: OCTargetCompilerParser ]
]
