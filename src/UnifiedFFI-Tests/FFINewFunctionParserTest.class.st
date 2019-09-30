Class {
	#name : #FFINewFunctionParserTest,
	#superclass : #FFIFunctionParserTest,
	#category : #'UnifiedFFI-Tests-Tests'
}

{ #category : #factory }
FFINewFunctionParserTest >> newParser [
	^ FFISplitFunctionParser new
]
