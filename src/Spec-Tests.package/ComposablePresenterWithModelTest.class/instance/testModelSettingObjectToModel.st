testing
testModelSettingObjectToModel

	| point model presenter |
	
	"we had a Model, new model is a regular object"
	model := TestingPointModel x: 1 y: 2.
	point := 40@54.
	presenter := TestingComposablePresenterWithModel on: model.
	self assert: presenter specCompatibleModel isSpecCompatibleModel.
	self deny: presenter specCompatibleModel isValueHolder.
	self deny: point isSpecCompatibleModel.
	self deny: point isValueHolder.
	presenter model: point.
	self assert: presenter specCompatibleModel isSpecCompatibleModel.
	self assert: presenter specCompatibleModel isValueHolder.
	self assert: presenter model == point.
	self deny: presenter specCompatibleModel == model.
	self assert: presenter specCompatibleModel announcer numberOfSubscriptions > 0.

