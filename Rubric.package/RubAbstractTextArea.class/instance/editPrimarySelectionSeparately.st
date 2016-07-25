accessing selection
editPrimarySelectionSeparately
	| view |
	view := self primarySelection readWriteView.
	(view embeddedInMorphicWindowLabeled: 'Selection editing') openInHand.