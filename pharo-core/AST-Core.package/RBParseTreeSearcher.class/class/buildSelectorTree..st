private
buildSelectorTree: aSelector 
	aSelector isEmpty ifTrue: [^nil].
	^RBParser parseRewriteExpression: '``@receiver ' 
				, (self buildSelectorString: aSelector)
		onError: [:err :pos | ^nil]