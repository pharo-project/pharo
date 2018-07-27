Extension { #name : #TraitedClass }

{ #category : #'*Deprecated70' }
TraitedClass >> addAndClassifySelector: selector withMethod: compiledMethod inProtocol: aCategory notifying: requestor [
	self
		deprecated: 'Please use #addAndClassifySelector:withMethod:inProtocol: instead'
		transformWith:
			'`@receiver addAndClassifySelector: `@statements1 withMethod: `@statements2 inProtocol: `@statements3 notifying: `@statements4'
		   -> '`@receiver addAndClassifySelector: `@statements1 withMethod: `@statements2 inProtocol: `@statements3'.
	self addAndClassifySelector: selector withMethod: compiledMethod inProtocol: aCategory
]
