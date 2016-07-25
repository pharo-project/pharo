spec protocol
okAction: aBlock

	^ self widget
		ifNotNil: [:w | w okAction: aBlock ]