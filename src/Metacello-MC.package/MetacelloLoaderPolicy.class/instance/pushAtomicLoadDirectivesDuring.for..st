actions
pushAtomicLoadDirectivesDuring: aBlock for: aLoader

	self pushLoadDirective: (MetacelloAtomicLoadDirective loader: aLoader) during: aBlock.