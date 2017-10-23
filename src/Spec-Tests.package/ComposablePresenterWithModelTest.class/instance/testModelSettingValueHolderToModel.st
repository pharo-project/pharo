testing
testModelSettingValueHolderToModel

	"we had a Model, new model is a value holder"
	| model point valueHolder presenter |
	point := 1@0.
	model := TestingPointModel x: 1 y: 2.
	valueHolder := NewValueHolder value: point.
	presenter := TestingComposablePresenterWithModel on: model.
	self assert: presenter announcingObject isAnnouncingObject.
	self deny: presenter announcingObject isValueHolder.
	self assert: valueHolder isAnnouncingObject.
	self assert: valueHolder isValueHolder.
	presenter model: valueHolder.
	self assert: presenter announcingObject isAnnouncingObject.
	self assert: presenter announcingObject isValueHolder.
	self assert: presenter announcingObject == valueHolder.
	self assert: presenter announcingObject announcer numberOfSubscriptions > 0.
