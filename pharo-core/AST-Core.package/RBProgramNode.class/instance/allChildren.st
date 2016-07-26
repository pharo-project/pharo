iterating
allChildren
	| children |
	children := OrderedCollection new.
	self nodesDo: [ :each | children addLast: each ].
	^ children