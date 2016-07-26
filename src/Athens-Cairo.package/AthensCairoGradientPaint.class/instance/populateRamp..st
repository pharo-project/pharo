private
populateRamp: aRamp
	aRamp do: [ :each |
		self
			addColorStopAt: each key
			colored: each value ]
	