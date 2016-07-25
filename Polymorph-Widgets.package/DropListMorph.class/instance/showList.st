protocol
showList
	"Show the list."

	self listMorph owner isNil
		ifTrue: [self positionList.
				self
					fillStyle: self fillStyleToUse;
					listPaneColor: self paneColor.
				self listMorph theme: self theme.
				self world addMorphInLayer: self listMorph.
				self listMorph wantsKeyboardFocus ifTrue: [
					self listMorph takeKeyboardFocus].
				self activeHand
					newMouseFocus: self listMorph]