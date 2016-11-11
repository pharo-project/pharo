actions
postload: loader package: packageSpec

	Author fullName: self class name.

	Smalltalk tools register: Workspace as: #browser.
	Smalltalk tools register: Workspace as: #monticelloBrowser.
	Smalltalk tools register: Workspace as: #fileList.
	Smalltalk tools register: Workspace as: #changeList.	
	
	Smalltalk ui theme settings preferGradientFill: true.
	
	RubScrolledTextMorph compile: 'font: aFont
	self textFont: aFont'.
	
	Author reset.
