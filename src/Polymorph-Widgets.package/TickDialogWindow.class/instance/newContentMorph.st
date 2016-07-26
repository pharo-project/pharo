display
newContentMorph

	self listModel: ((self tickListClass 
						itemsList: self itemsList 
						itemsHeaderName: self itemsHeaderName 
						wrapBlockOrSelector: self wrapBlockOrSelector)
						autoMultiSelection: true;
						beCheckList;
						yourself).
	
	self listMorph: (self listModel defaultTreeMorph
								buildContents;
								vResizing: #spaceFill;
								hResizing: #spaceFill).
								
	self defaultValue ifTrue: [ self listMorph selectAll ].							
	^ self listMorph