*OpalCompiler-Core
rewriteTempsForContext: aContext
	| rewriter contextOnlyTemps |
	rewriter := RBParseTreeRewriter new.
	contextOnlyTemps := self notShadowedTemporaryNamesFrom: aContext.
	aContext tempNames
		withIndexDo:
			[ :tempName :index | 
			(contextOnlyTemps includes: tempName)
				ifTrue:
					[ rewriter
						replace: tempName , ' := ``@object' with: 'ThisContext namedTempAt:' , index asString , 'put: ``@object';
						replace: tempName with: 'ThisContext namedTempAt:' , index asString ] ].
	^ rewriter
		executeTree: self;
		tree