instance creation
newFrom: aCompiledMethod
	| inst |
	inst := super basicNew: aCompiledMethod size.
	1 to: aCompiledMethod size do: [:index |
		inst at: index put: (aCompiledMethod at: index)].
	^ inst.