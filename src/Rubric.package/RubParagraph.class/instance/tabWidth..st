accessing
tabWidth: anInteger
	| newTextStyle tabsArray |
	newTextStyle := self textStyle copy.
	tabsArray := (anInteger to: 99999 by: anInteger) asArray.
	newTextStyle privateTabsArray: tabsArray.
	self textArea privateSetTextStyle: newTextStyle.
	self releaseComposer.
	self compose.
	self textArea recomputeSelection; changed