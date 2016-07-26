private
titleOrIconOf: aPresentation in: container
	^ aPresentation hasTitleIcon 
		ifTrue: [
			| image |
			image := GLMInteractableBrick new
						normalImage: aPresentation titleIconValue.
			image maxWidth: [ aPresentation titleIconValue width * 2 ].
			image hSpaceFill.
			image vSpaceFill.
			aPresentation titleValue ifNotNil: [
				image setBalloonText: aPresentation titleValue maxLineLength: 100 ].
			image  ]
		ifFalse: [ aPresentation titleValue ifNil: [''] ]