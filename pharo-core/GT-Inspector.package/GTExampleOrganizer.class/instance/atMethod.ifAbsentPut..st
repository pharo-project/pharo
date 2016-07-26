accessing
atMethod: aCompiledMethod ifAbsentPut: aBlock
	<todo: '#theNonMetaClass is not good since we could not put examples to the instance and class-side at once'>
	^ (examples 
		at: aCompiledMethod methodClass theNonMetaClass name 
		ifAbsentPut: [ WeakIdentityKeyDictionary new ]) 
			at: aCompiledMethod selector 
			ifAbsentPut: aBlock