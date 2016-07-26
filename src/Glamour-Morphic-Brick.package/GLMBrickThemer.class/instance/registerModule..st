brick-modules
registerModule: aModule

	^ self
		registerModule: aModule
		to: self
		named: thisContext sender selector