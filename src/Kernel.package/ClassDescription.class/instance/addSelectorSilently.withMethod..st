accessing method dictionary
addSelectorSilently: selector withMethod: compiledMethod
	super addSelectorSilently: selector withMethod: compiledMethod.
	self instanceSide noteAddedSelector: selector meta: self isMeta.