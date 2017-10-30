testing
testModelSettingModelToValueHolder

	| point model presenter |
	
	"we had value holder, new model is a model"
	point := 40@54.

	presenter := TestingComposablePresenterWithModel on: point.
	model := TestingPointModel x: 1 y: 2.
	self assert: presenter announcingObject isAnnouncingObject.
	self assert: presenter announcingObject isValueHolder.
	self assert: model isAnnouncingObject.
	self deny: model isValueHolder.
	presenter model: model.
	self assert: presenter announcingObject isAnnouncingObject.
	self deny: presenter announcingObject isValueHolder.
	self assert: presenter announcingObject == model.
	self assert: presenter announcingObject announcer numberOfSubscriptions > 0.

