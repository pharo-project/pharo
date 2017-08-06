drawing
drawLinesOn: canvas
	"This method draws the line below the tab-label, to make it look as 'behind' (when there is one 
	 selected, it draws that line except on the corresponding selected tab."
	self flag: #TODO. "This is probably the worst method name ever, find a better one"
	self selectedTab 
		ifNotNil: [ self drawWithSelectionOn: canvas ]	
		ifNil: [ self drawWithoutSelectedOn: canvas ]
