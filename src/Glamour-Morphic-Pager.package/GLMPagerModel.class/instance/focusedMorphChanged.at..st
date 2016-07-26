focus
focusedMorphChanged: aMorph at: pageIndex
	"call this method if a morph was focused in a pane with specified index. As a result corresponding pane will be marked as selected"
	
	
	self assignFocusableMorph: aMorph withPaneAt: pageIndex.
	
	focusedPageIndex = pageIndex ifFalse: [ 
		focusedPageIndex := pageIndex.
		self notifyFocusedPageChanged ]
	