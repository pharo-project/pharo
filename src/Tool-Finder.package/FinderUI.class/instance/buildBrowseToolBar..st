items creation
buildBrowseToolBar: aWindow
	| toolbar |
	toolbar := aWindow newToolbar: {
		self buildBrowseButton.
		self buildSendersButton.
		self buildImplementorsButton.
		self buildVersionsButton.
		self buildInheritanceButton.
		self buildHierarchyButton}.
	^toolbar hResizing: #shrinkWrap