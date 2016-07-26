adding
removePage: aMorph
	"Remove the given page and its tab."

	|index|
	index := self pages indexOf: aMorph.
	self pages remove: aMorph.
	self tabSelectorMorph removeTabIndex: index