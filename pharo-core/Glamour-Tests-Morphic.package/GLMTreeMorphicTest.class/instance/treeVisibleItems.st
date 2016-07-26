private
treeVisibleItems
	"Returns the roots of the tree included in the current window. It looks like morphic creates new collections when the tree is refreshed which means different calls to this method may return different objects."
	^ self tree scroller submorphs