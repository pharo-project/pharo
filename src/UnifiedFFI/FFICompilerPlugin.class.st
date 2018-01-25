Class {
	#name : #FFICompilerPlugin,
	#superclass : #OCCompilerASTPlugin,
	#category : #UnifiedFFI
}

{ #category : #api }
FFICompilerPlugin >> transform [
	| rule pattern |
	pattern := '`@message ffiCall: ``@args'.
	rule := RBParseTreeSearcher new
		matchesAnyOf: {pattern}
		do: [ :node :answer | ast propertyAt: #methodProperties ifAbsentPut: [ FFIAdditionalFFIMethodState new: 0 ] ].
	rule executeTree: ast.
	ast propertyAt: #methodProperties ifPresent: [ :methodState | methodState argsNames: ast argumentNames ] ifAbsent: [  ].
	^ ast
]
