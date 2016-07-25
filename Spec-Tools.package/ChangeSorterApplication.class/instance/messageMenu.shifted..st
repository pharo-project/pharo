menu
messageMenu: aMenu shifted: shifted
	"Fill aMenu with items appropriate for the message list; could be for a single or double changeSorter"

	self selectedSelector ifNil: [ ^ nil ].
	aMenu addTitle: 'Message list'.
	aMenu addAllFromPragma: 'changeSorterMessageListMenu' target: self.
	^ aMenu