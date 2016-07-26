updating
updatePageAtIndex: anInteger withLabel: aStringOrMorph toolbar: aToolbar action: anActionMorph
	
	(self pages at: anInteger)
			toolbar: aToolbar.
	self 
		relabelPage: (self pages at: anInteger)
		with: (self 
			buildLabelFrom: aStringOrMorph 
			withAction: anActionMorph).
	(self tabSelectorMorph selectedIndex = anInteger) ifTrue: [  
		self updatePageIndex: anInteger ]
