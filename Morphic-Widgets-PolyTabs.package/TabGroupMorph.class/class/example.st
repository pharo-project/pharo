example
example
	| builder window page1 page2 label1 label2 tabs content okButton cancelButton |
	builder := self theme builder.
	okButton := builder newOKButton.
	cancelButton := builder newCancelButton.
	page1 := (builder
		newStack:
		{((builder
		newAlphaImage: (Smalltalk ui icons iconNamed: #warningIcon)
		help: nil) alpha: 0.5).
	(CircleMorph new
		hResizing: #spaceFill;
		vResizing: #spaceFill)})
		fillStyle: Color red;
		hResizing: #spaceFill;
		vResizing: #spaceFill.
	page2 := builder newPanel
		fillStyle: Color green;
		hResizing: #spaceFill;
		vResizing: #spaceFill.
	label1 := builder
		newRow:
			{(builder newButtonLabel: 'First page').
			(builder
				newCloseControlFor: nil
				action: [ tabs removePage: page1 ]
				help: 'Close this tab')}.
	label2 := builder
		newRow:
			{(builder newButtonLabel: 'Second page').
			(builder
				newCloseControlFor: nil
				action: [ tabs removePage: page2 ]
				help: 'Close this tab')}.
	label1 cellInset: 0.
	label2 cellInset: 0.
	tabs := builder
		newTabGroup:
			{(label1 -> page1).
			(label2 -> page2)}.
	content := builder
		newColumn:
			{tabs.
			((builder
				newRow:
					{okButton.
					cancelButton}) listCentering: #bottomRight)}.
	window := content
		extent: 250 @ 300;
		openInWindow.
	okButton
		model: window;
		action: #delete.
	cancelButton
		model: window;
		action: #delete