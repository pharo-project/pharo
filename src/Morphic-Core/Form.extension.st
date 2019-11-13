Extension { #name : #Form }

{ #category : #'*Morphic-Core' }
Form >> anyShapeFill [
	"Fill the interior of the outermost outlined region in the receiver, a 1-bit deep form.  Typically the resulting form is used with fillShape:fillColor: to paint a solid color.  See also convexShapeFill:"

	| shape |
	"Draw a seed line around the edge and fill inward from the outside."
	shape := self findShapeAroundSeedBlock: [:f | f borderWidth: 1].
	"Reverse so that this becomes solid in the middle"
	shape := shape reverse.
	"Finally erase any bits from the original so the fill is only elsewhere"
	shape copy: shape boundingBox from: self to: 0@0 rule: Form erase.
	^ shape
]

{ #category : #'*Morphic-Core' }
Form >> borderWidth: anInteger [ 
	"Set the width of the border for the receiver to be anInteger and paint it 
	using black as the border color."

	self 
		border: self boundingBox 
		width: anInteger 
		fillColor: Smalltalk ui theme borderColor
]

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
