morphic ui
openOn: aFileReference titled: aTitle
	
	| listView |
	listView := self newOnDirectory: aFileReference baseLabel: aTitle.
	listView morphicView openInWorld.
	^ listView