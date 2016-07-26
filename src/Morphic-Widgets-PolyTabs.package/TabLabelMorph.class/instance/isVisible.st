testing
isVisible
	"Answer whether the tab is currently visible."

	^self tabSelector
		ifNil: [false]
		ifNotNil: [:selector | selector visibleTabs includes: self]