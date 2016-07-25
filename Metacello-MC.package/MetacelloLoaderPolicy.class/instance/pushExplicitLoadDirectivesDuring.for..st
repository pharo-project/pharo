actions
pushExplicitLoadDirectivesDuring: aBlock for: aLoader

	| directive |
	directive := MetacelloExplicitLoadDirective loader: aLoader.
	self pushLoadDirective: directive during: aBlock.
	^directive