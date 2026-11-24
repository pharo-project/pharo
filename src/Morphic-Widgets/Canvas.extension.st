Extension { #name : 'Canvas' }

{ #category : '*Morphic-Widgets' }
Canvas >> copyClipRect: newClipRect [

	^ ClippingCanvas canvas: self clipRect: newClipRect
]
