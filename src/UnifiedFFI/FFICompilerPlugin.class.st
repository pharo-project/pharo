Class {
	#name : #FFICompilerPlugin,
	#superclass : #OCCompilerASTPlugin,
	#category : #UnifiedFFI
}

{ #category : #'as yet unclassified' }
FFICompilerPlugin class >> priority [
	^ 2
]

{ #category : #api }
FFICompilerPlugin >> transform [
	| rule patterns |
	patterns := {'`@message ffiCall: ``@args1' . '`@message ffiCall: ``@args1 module: ``@args2'.
	'`@message ffiCall: ``@args1  module: ``@args2 options: ``@args3'.
	'`@message ffiCall: ``@args1 options: ``@args2'.
	'`@message call: ``@args1'.
	'`@message call: ``@args1 options: ``@args2'.
	'`@message callUnchecked: ``@args1 options: ``@args2'}.
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
