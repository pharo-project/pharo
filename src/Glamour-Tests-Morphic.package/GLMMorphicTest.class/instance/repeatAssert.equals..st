private
repeatAssert: aBlock equals: aResult

	self assert: (self wait: 0.5 until: aBlock evaluatesTo: aResult)