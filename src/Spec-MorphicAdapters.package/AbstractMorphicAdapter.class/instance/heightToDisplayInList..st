dispatch
heightToDisplayInList: aList
	"Return the width of my representation as a list item"
	
	self model extent ifNotNil: [:ex | ^ ex y ].
	self model initialExtent ifNotNil: [:ex | ^ ex y ].
	
	self widget ifNil: [ self buildWithSpec ].
	self widget 
		vResizing: #rigid;
		hResizing: #rigid.
		
	^ self widget heightToDisplayInList: aList