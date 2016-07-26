*GT-Spotter
spotterProcessorsFor: aSpotterStep
	"This is a utility method that collects all extensions (processors) for the current object.
	By default, it simply looks for the #spotterOrder: pragma.
	The step can decice whether or not an extension should be enabled.
	The step can also configure each extension (e.g. override any property)"
	
	^ (((Pragma 
		allNamed: #spotterOrder:
		from: self class 
		to: Object)
			select: [:pragma | aSpotterStep processorsFilter shouldEnableProcessorsCreateBy: pragma method])
			sort: [ :first :second | (first argumentAt: 1) < (second argumentAt: 1) ])
			do: [ :pragma ||currentSize|
				currentSize := aSpotterStep processors size.
				self spotterProcessorsFor: aSpotterStep pragma: pragma.
				aSpotterStep "configure the new processors if required"
					configureProcessorsStartingAt: currentSize+1 
					with: (aSpotterStep processorsFilter configurationBlockForProcessorsCreatedBy: pragma method)]