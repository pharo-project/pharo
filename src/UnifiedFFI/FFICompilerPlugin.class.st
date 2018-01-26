"
I am a FFICompilerPlugin. I am a plugin for the compiler that make the compiled method store the arguments names to be used for FFI when the sources are not loaded or unloaded.

See also FFIAdditionalFFIMethodState and FDBDecompiler>>createNArgs:
"
Class {
	#name : #FFICompilerPlugin,
	#superclass : #OCCompilerASTPlugin,
	#category : #UnifiedFFI
}

{ #category : #accessing }
FFICompilerPlugin class >> priority [
	^ 2
]

{ #category : #accessing }
FFICompilerPlugin >> convertKeywordsMessagesToRBSyntax: aCollectionOfKeywordMessages [
	^ aCollectionOfKeywordMessages
		collect: [ :aMessage | 
			String
				streamContents: [ :aStream | 
					aStream
						<< '`@message';
						space.
					(aMessage splitOn: ':') allButLast
						withIndexDo: [ :aKeyword :i | 
							(aStream << aKeyword << ': ``@args')
								<< i;
								space ] ] ]
]

{ #category : #accessing }
FFICompilerPlugin >> messagesToLookup [
	| collector |
	collector := PragmaCollector filter: [ :pragma | pragma keyword = #ffiCallMethod ].
	^ (collector reset collect: [ :aPragma | aPragma method selector ]) asSet
]

{ #category : #api }
FFICompilerPlugin >> transform [
	| rule patterns |
	patterns := self convertKeywordsMessagesToRBSyntax: self messagesToLookup.
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
