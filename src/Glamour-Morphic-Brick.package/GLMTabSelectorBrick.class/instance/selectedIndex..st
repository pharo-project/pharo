accessing
selectedIndex: index
	"Set the value of selectedIndex"

	|oldIndex|
	self selectedIndex == index ifTrue: [^ self ].
	oldIndex := self selectedIndex.
	selectedIndex := (index min: self tabs size).
	self updateTabs.
	self changed: #selectedIndex with: oldIndex