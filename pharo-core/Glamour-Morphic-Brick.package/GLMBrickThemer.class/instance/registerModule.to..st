brick-modules
registerModule: aModule to: aThemer

	^ self
		registerModule: aModule
		to: aThemer
		named: thisContext sender selector