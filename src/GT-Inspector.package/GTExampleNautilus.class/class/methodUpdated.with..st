private
methodUpdated: aCompiledMethod with: aResult
	[	MethodWidget methodsIconsCache 
			at: aCompiledMethod 
			put: (GTExampleNautilusAction iconForMethod: aCompiledMethod result: aResult). 
		NautilusUI allSubInstances do: #updateCategoryAndMethodList ]
			on: Error
			do: [ " ignore and continue " ]