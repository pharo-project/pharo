Extension { #name : #Form }

{ #category : #'*Morphic-Core' }
Form >> defaultCanvasClass [

	"Return the default canvas used for drawing onto the receiver"
	^ FormCanvas
]

{ #category : #'*Morphic-Core' }
Form >> getCanvas [
	"Return a Canvas that can be used to draw onto the receiver"
	^self defaultCanvasClass on: self
]
