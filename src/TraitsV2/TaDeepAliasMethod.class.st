Class {
	#name : #TaDeepAliasMethod,
	#superclass : #TaAliasMethod,
	#category : #'TraitsV2-Compositions'
}

{ #category : #testing }
TaDeepAliasMethod >> changesSourceCode: aSelector [ 
	
	^ true
]

{ #category : #rewriting }
TaDeepAliasMethod >> rewriteText: aSelector [
	| stream |
	aSelector numArgs == 0
		ifTrue: [ ^ 'self ' , aSelector asString ].

	stream := WriteStream on: String new.
	stream nextPutAll: 'self '.

	aSelector keywords
		withIndexDo: [ :each :i | 
			stream
				nextPutAll: each;
				nextPutAll: ' ``@arg';
				nextPutAll: i printString;
				nextPut: Character space ].
	^ stream contents
]

{ #category : #accessing }
TaDeepAliasMethod >> sourceCodeAt: aSelector [
	| originalSourceCode parseTree rewriter |
	originalSourceCode := super sourceCodeAt: aSelector.
	parseTree := RBParser parseMethod: originalSourceCode.
	rewriter := RBParseTreeRewriter new.
	
	self aliases
		associationsDo:
			[ :assoc | rewriter replace: (self rewriteText: assoc value) with: (self rewriteText: assoc key) ].
	
	^ (rewriter
			executeTree: parseTree;
			tree) formattedCode
]
