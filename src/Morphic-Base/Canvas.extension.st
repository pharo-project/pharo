Extension { #name : #Canvas }

{ #category : #'*Morphic-Base' }
Canvas >> asAlphaBlendingCanvas: alpha [
	^(AlphaBlendingCanvas on: self) alpha: alpha
]

{ #category : #'*Morphic-Base' }
Canvas >> copyClipRect: newClipRect [

	^ ClippingCanvas canvas: self clipRect: newClipRect

]
