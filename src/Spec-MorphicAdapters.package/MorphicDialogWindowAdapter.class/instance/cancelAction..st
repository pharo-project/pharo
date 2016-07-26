spec protocol
cancelAction: aBlock

	^ self widget
		ifNotNil: [:w | w cancelAction: aBlock ]