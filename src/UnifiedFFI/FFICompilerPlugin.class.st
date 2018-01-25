Class {
	#name : #FFICompilerPlugin,
	#superclass : #OCCompilerASTPlugin,
	#category : #UnifiedFFI
}

{ #category : #api }
FFICompilerPlugin >> transform [
	| rule patterns |
	patterns := {'`@message ffiCall: ``@args' . '`@message ffiCall: ``@args module: ``@args' .
	'`@message ffiCall: ``@args  module: ``@args options: ``@args' . '`@message ffiCall: ``@args options: ``@args'}.
	rule := RBParseTreeSearcher new
		matchesAnyOf: patterns
		do:
			[ :node :answer | ast propertyAt: #methodProperties ifAbsentPut: [ FFIAdditionalFFIMethodState new: 0 ] ].
	rule executeTree: ast.
	ast
		propertyAt: #methodProperties
		ifPresent: [ :methodState | methodState argsNames: ast argumentNames ]
		ifAbsent: [  ].
	^ ast
]
