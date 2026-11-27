Extension { #name : 'Canvas' }

{ #category : '*Morphic-Widgets' }
Canvas >> copyClipRect: newClipRect [

	^ ClippingCanvas canvas: self clipRect: newClipRect
]

{ #category : '*Morphic-Widgets' }
Canvas >> rubParagraph: paragraph bounds: bounds color: c [
	"Draw the given paragraph"
	^self subclassResponsibility
]
