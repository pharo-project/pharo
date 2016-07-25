windows
onWindowLabelChanged: ann
	self tabGroup relabelPage: ann window with: (self tabLabelFor: ann window)  