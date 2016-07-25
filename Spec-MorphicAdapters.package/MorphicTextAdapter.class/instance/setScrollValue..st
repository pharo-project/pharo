widget API
setScrollValue: aValue
	self
		widgetDo: [ :w | 
			w scrollPane
				hScrollbarValue: aValue x;
				vScrollbarValue: aValue y;
				updateScrollbars ]