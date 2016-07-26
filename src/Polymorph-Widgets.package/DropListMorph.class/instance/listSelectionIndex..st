accessing
listSelectionIndex: anInteger
	"Set the list selection."

	self hideList.
	anInteger = 0 ifTrue: [^self].
	listSelectionIndex := anInteger.
	self
		changed: #listSelectionIndex;
		updateContents;
		triggerEvent: #selectionIndex with: anInteger.
	self model ifNotNil: [:m | 
		self setIndexSelector ifNotNil: [:s |
			self useSelectionIndex
				ifTrue: [m perform: s with: anInteger]
				ifFalse: [m perform: s with: self selectedItem]]]