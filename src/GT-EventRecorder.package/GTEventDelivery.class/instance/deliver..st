delivery
deliver: aBoolean
	| bundles |
	bundles := GTEventMultiBundleFactory fromBundles: self bundles.
	bundles do: [ :eachGTEventMultiBundle |
		self deliver: eachGTEventMultiBundle signalProgress: aBoolean ]