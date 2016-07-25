panes
popAndReplacePane: aMorph
	
	self doNotNotifyDuring: [ :m |
		m popPane;
			pushPane: aMorph smooth: false ].
		
	self notifyPagePoppedAndReplaced: aMorph