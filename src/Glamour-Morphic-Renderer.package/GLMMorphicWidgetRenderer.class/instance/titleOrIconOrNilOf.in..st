private
titleOrIconOrNilOf: aPresentation in: container
	^ aPresentation hasTitleIcon 
		ifTrue: [
			| image |
			image := self theme 
					newImageIn: container
					form: aPresentation titleIconValue.
			aPresentation titleValue isNil ifFalse: [
				image setBalloonText: aPresentation titleValue maxLineLength: 10 ]  ]
		ifFalse: [ 
			aPresentation hasTitle 
				ifTrue: [ aPresentation titleValue ]
				ifFalse: [ nil ] ]