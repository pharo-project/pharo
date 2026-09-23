Extension { #name : 'CompiledMethod' }

{ #category : '*DeepCopier' }
CompiledMethod >> veryDeepCopyWith: deepCopier [
	"Return self.  I am always shared.  Do not record me.  Only use this for blocks.  Normally methodDictionaries should not be copied this way."
]
