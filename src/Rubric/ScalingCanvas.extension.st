Extension { #name : #ScalingCanvas }

{ #category : #'*Rubric-Editing-Core' }
ScalingCanvas >> rubParagraph: paragraph bounds: bounds color: c [

	| clippedCanvas currentParagraph |
	
	clippedCanvas := self copyClipRect: bounds.
	currentParagraph := paragraph.
	[ currentParagraph next isNil ] whileFalse: [
		(currentParagraph canDrawDecoratorsOn: clippedCanvas) ifFalse: [ ^ self ].
		currentParagraph drawOn: clippedCanvas.
		currentParagraph := currentParagraph next ].
	formCanvas rubParagraph: currentParagraph bounds: (bounds scaleBy: scale) color: c scale: scale
]
