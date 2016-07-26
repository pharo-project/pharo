rectangle functions
areasOutside: aRectangle 
	"Answer an Array of Rectangles comprising the parts of the receiver not 
	intersecting aRectangle."
	"Make sure the intersection is non-empty"
	
	(self intersects: aRectangle) ifFalse: [ ^ Array with: self ].
	^ Array new: 4 streamContents: [ :aStream |
		| yOrigin yCorner |
		aRectangle origin y > origin y 
			ifTrue:
				[ aStream nextPut: (origin corner: corner x @ (yOrigin := aRectangle origin y)) ]
			ifFalse: [ yOrigin := origin y ].
		aRectangle corner y < corner y 
			ifTrue: 
				[ aStream nextPut: (origin x @ (yCorner := aRectangle corner y) corner: corner) ]
			ifFalse: [ yCorner := corner y ].
		aRectangle origin x > origin x ifTrue: 
			[ aStream nextPut: (origin x @ yOrigin corner: aRectangle origin x @ yCorner) ].
		aRectangle corner x < corner x ifTrue: 
			[ aStream nextPut: (aRectangle corner x @ yOrigin corner: corner x @ yCorner) ] ]