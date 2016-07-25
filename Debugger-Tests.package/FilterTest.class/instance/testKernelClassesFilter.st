tests
testKernelClassesFilter
	| kernelClassesFilter setContext |
	
	setContext := session stepInto stepInto stack first.
	kernelClassesFilter := KernelClassesFilter new.
	self assert: (kernelClassesFilter shouldDisplay: context).
	self assert: (kernelClassesFilter shouldDisplay: nilContext).
	self deny: (kernelClassesFilter shouldDisplay: setContext)