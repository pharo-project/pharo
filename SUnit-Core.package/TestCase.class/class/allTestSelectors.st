accessing
allTestSelectors
	| answer pivotClass lookupRoot |
	answer := Set withAll: self testSelectors.
	self shouldInheritSelectors
		ifTrue:
			[pivotClass := self.
			lookupRoot := self lookupHierarchyRoot.
			[pivotClass == lookupRoot]
				whileFalse:
					[pivotClass := pivotClass superclass.
					answer addAll: pivotClass testSelectors]].
	^answer asSortedCollection asOrderedCollection