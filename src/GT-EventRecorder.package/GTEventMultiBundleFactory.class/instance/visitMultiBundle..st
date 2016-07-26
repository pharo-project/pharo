visitor
visitMultiBundle: aMultiBundle
	aMultiBundle bundles do: [ :eachSingleBundle |
		self visitSingleBundle: eachSingleBundle ]