actions
pushLinearLoadDirectivesDuring: aBlock for: aLoader

	self pushLoadDirective: (MetacelloLinearLoadDirective loader: aLoader) during: aBlock.