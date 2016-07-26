building
accumulatedSourceOf: m for: object in: composite
	composite accumulator with: [ :acc | 
		acc show: [ :a |
			a title: [ :method | method selector ].
			self sourceOf: m for: object in: a ] ]