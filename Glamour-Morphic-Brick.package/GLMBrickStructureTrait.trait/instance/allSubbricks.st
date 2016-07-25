brick-structure
allSubbricks
	| all |
	all := OrderedCollection new.
	self allSubbricksDo: [:each | all add: each ].
	^ all