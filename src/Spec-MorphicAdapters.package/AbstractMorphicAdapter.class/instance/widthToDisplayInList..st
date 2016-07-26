dispatch
widthToDisplayInList: aList
	"Return the width of my representation as a list item"
	
	self model extent ifNotNil: [:ex | ^ ex x ].
	self model initialExtent ifNotNil: [:ex | ^ ex x ].

	self widget ifNil: [ self buildWithSpec ].
	self widget 
		vResizing: #rigid;
		hResizing: #rigid.
		
	^ self widget widthToDisplayInList: aList