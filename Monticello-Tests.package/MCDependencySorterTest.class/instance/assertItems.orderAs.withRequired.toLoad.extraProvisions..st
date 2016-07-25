asserting
assertItems: anArray orderAs: depOrder withRequired: missingDeps toLoad: unloadableItems extraProvisions: provisions
	| order sorter items missing unloadable |
	items := anArray collect: [:ea | self itemWithSpec: ea].
	sorter := MCDependencySorter items: items.
	sorter addExternalProvisions: provisions.
	order := (sorter orderedItems collect: [:ea | ea name]) asArray.
	self assert: order = depOrder.
	missing := sorter externalRequirements.
	self assert: missing asSet = missingDeps asSet.
	unloadable := (sorter itemsWithMissingRequirements collect: [:ea | ea name]) asArray.
	self assert: unloadable asSet = unloadableItems asSet