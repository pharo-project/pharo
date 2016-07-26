operations
ensureSelectedTabIsVisible
	"Update the tabs if the seleted tab is not in the visible range."

	(self selectedIndex > 0 and: [(self visibleTabRange includes: self selectedIndex) not])
		ifTrue: [self visibleTabRange: (self selectedIndex to: 0).
				self updateTabs]