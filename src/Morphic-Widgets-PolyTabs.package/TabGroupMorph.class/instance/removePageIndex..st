page
removePageIndex: anInteger
	"Remove the page at index anInteger page and its tab."

	self pages removeAt: anInteger.
	self tabSelectorMorph removeTabIndex: anInteger