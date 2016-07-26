ui elements
fontPreviewPanel 

	^fontPreviewPanel ifNil:
		[ fontPreviewPanel := self newScrollPaneFor: self newFontPreviewInnerPanel ]