brick-structure
allSubbricksDo: aBlock

	self subbricks do: [:each | 
		each isBrick ifTrue: [ aBlock value: each. each allSubbricksDo: aBlock ] ]