brick-tab-label-testing
isSelected
	"Answer whether the tab is selected."

	^ self tabSelector
		ifNil: [ false ]
		ifNotNil: [ :selector | selector selectedTab == self]