testing
testUpdateModel

	| aPoint presenter |
	
	aPoint := TestingPointModel x: 40 y: 54.
	presenter := TestingComposablePresenterWithModel on: aPoint.
	
	self assert: presenter x text equals: aPoint x asString.
	self assert: presenter y text equals: aPoint y asString.
	
	aPoint x: 1; y: 2.
	aPoint valueChanged.
	
	"the point itself is not subscribed to the presenter"

	self assert: (presenter x text = aPoint x asString).
	self assert: (presenter y text = aPoint y asString).
	
	
	