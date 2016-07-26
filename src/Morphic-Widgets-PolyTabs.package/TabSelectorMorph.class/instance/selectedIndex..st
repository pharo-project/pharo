accessing
selectedIndex: index
	"Set the value of selectedIndex"

	|oldIndex|
	selectedIndex == index ifTrue: [^self].
	oldIndex := selectedIndex.
	selectedIndex := (index min: self tabs size).
	self visibleTabRange first > index
		ifTrue: [self visibleTabRange: ((index max: 1) to: 0)]
		ifFalse: [self visibleTabRange last < index
				ifTrue: [self visibleTabRange: (0 to: index)]].
	self updateTabs.
	self changed: #selectedIndex with: oldIndex