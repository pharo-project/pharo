accessing
children
	| children |
	children := OrderedCollection new.
	self childrenDo: [:each | children add: each].
	^children