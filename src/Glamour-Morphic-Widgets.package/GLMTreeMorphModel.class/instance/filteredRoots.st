accessing
filteredRoots
	| filtered |
	roots ifNil: [ self roots: self displayValue ].
	filtered := roots select: [ :each | each shouldBeDisplayed ].
	^ filtered