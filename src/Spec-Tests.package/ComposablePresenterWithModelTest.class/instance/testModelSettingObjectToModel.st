testing
testModelSettingObjectToModel

	| point model presenter |
	
	"we had a Model, new model is a regular object"
	model := TestingPointModel x: 1 y: 2.
	point := 40@54.
	presenter := TestingComposablePresenterWithModel on: model.
	self assert: presenter announcingObject isAnnouncingObject.
	self deny: presenter announcingObject isValueHolder.
	self deny: point isAnnouncingObject.
	self deny: point isValueHolder.
	presenter model: point.
	self assert: presenter announcingObject isAnnouncingObject.
	self assert: presenter announcingObject isValueHolder.
	self assert: presenter model == point.
	self deny: presenter announcingObject == model.
	self assert: presenter announcingObject announcer numberOfSubscriptions > 0.

