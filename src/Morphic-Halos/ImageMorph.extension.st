Extension { #name : 'ImageMorph' }

{ #category : '*Morphic-Halos' }
ImageMorph >> addCustomMenuItems: aMenu hand: aHand [
	super addCustomMenuItems: aMenu hand: aHand.
	aMenu addUpdating: #opacityString selector: #changeOpacity
]

{ #category : '*Morphic-Halos' }
ImageMorph >> changeOpacity [
	self isOpaque: self isOpaque not
]

{ #category : '*Morphic-Halos' }
ImageMorph >> grabFromScreen [

	self form: Screenshot new formScreenshotFromUserSelection
]

{ #category : '*Morphic-Halos' }
ImageMorph >> opacityString [
	^ (self isOpaque) -> 'opaque' translated
]

{ #category : '*Morphic-Halos' }
ImageMorph >> readFromFile [
	| fileName |
	fileName := self morphicUIManager
		request: 'Please enter the image file name' translated
		initialAnswer: 'fileName'.
	fileName isEmptyOrNil ifTrue: [^ self].
	self form: (Form fromFileNamed: fileName)
]
