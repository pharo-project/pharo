Extension { #name : 'Canvas' }

{ #category : '*Morphic-Base' }
Canvas >> copyClipRect: newClipRect [

	^ ClippingCanvas canvas: self clipRect: newClipRect
]
