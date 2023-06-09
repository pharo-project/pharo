Extension { #name : #Color }

{ #category : #'*Athens-Core' }
Color >> asAthensPaintOn: anAthensCanvas [

	^ self

	"^ anAthensCanvas surface createSolidColorPaint: self"
]

{ #category : #'*Athens-Core' }
Color >> asStrokePaintOn: aCanvas [
	^ aCanvas surface createStrokePaintFor: self
]

{ #category : #'*Athens-Core' }
Color >> athensFillPath: aPath on: aCanvas [
	^ aCanvas surface fillPath: aPath withSolidColor:self
]

{ #category : #'*Athens-Core' }
Color >> athensFillRectangle: aRect on: anAthensCanvas [

	^ anAthensCanvas surface fillRectangle: aRect withSolidColor:self
]

{ #category : #'*Athens-Core' }
Color >> strokePath: aPath on: aCanvas strokeWidth: width [
	^ (aCanvas surface createSolidColorPaint: self) strokePath: aPath on: aCanvas strokeWidth: width
]

{ #category : #'*Athens-Core' }
Color >> strokeRect: aRect on: aCanvas strokeWidth: width [
	^ (aCanvas surface createSolidColorPaint: self) strokeRect: aRect on: aCanvas strokeWidth: width
]
