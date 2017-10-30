testing
testUpdateModelWithValueHolder

	| aPoint aValueHolder presenter anAnnouncer |
	
	"create a point and a value holder that contains it and can react on announcements"
	aPoint := 40@54.
	aValueHolder := NewValueHolder value: aPoint.
	
	self assert: (aValueHolder value == aPoint).
	anAnnouncer := aValueHolder announcer.
	self assert: (aValueHolder announcer subscriptions subscriptions size) equals: 0. 
	
	presenter := TestingComposablePresenterWithModel on: aValueHolder.

	self assert: (anAnnouncer == aValueHolder announcer).
	self assert: presenter x text equals: aPoint x asString.
	self assert: presenter y text equals: aPoint y asString.
	
	aPoint setX: 1 setY: 2.
	self assert: (aValueHolder value == aPoint). 
	aValueHolder valueChanged.
	self assert: (anAnnouncer == aValueHolder announcer).
	
	"the point itself is not subscribed to the presenter"

	self assert: presenter x text equals: aPoint x asString.
	self assert: presenter y text equals: aPoint y asString.
	
	
	