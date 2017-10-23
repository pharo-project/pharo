testing
testModelSettingValueHolderToValueHolder

	| point point2 valueHolder presenter |
	
	"we had value holder, new model is a regular object"
	point := 40@54.
	point2 := 1@0.
	
	"we had value holder, new model is a value holder"
	presenter := TestingComposablePresenterWithModel on: point.
	valueHolder := NewValueHolder value: point2.
	self assert: presenter announcingObject isAnnouncingObject.
	self assert: presenter announcingObject isValueHolder.
	self deny: point2 isAnnouncingObject.
	self assert: valueHolder isValueHolder.
	presenter model: valueHolder.
	self assert: presenter announcingObject isAnnouncingObject.
	self assert: presenter announcingObject isValueHolder.
	self assert: presenter model == point2.
	self assert: presenter announcingObject == valueHolder.
	self assert: presenter announcingObject announcer numberOfSubscriptions > 0.
