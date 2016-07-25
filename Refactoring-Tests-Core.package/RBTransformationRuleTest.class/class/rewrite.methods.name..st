instance creation
rewrite: stringArrays methods: aBoolean name: aName 
	| rewriteRule |
	rewriteRule := RBParseTreeRewriter new.
	stringArrays do: 
			[:each | 
			aBoolean
				ifTrue: [rewriteRule addMethodSearch: each first -> each last]
				ifFalse: [rewriteRule addSearch: each first -> each last]].
	^(self new) name: aName;
		rewriteUsing: rewriteRule;
		yourself