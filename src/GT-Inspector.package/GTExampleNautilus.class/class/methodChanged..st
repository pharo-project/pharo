private
methodChanged: aCompiledMethod
	[	MethodWidget methodsIconsCache removeKey: aCompiledMethod. 
		NautilusUI allSubInstances do: #updateCategoryAndMethodList ]
			on: Error
			do: [ " ignore and continue " ]