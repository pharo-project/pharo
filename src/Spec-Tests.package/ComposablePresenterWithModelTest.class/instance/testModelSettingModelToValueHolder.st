testing
testModelSettingModelToValueHolder

	| point model presenter |
	
	"we had value holder, new model is a model"
	point := 40@54.

	presenter := TestingComposablePresenterWithModel on: point.
	model := TestingPointModel x: 1 y: 2.
	self assert: presenter specCompatibleModel isSpecCompatibleModel.
	self assert: presenter specCompatibleModel isValueHolder.
	self assert: model isSpecCompatibleModel.
	self deny: model isValueHolder.
	presenter model: model.
	self assert: presenter specCompatibleModel isSpecCompatibleModel.
	self deny: presenter specCompatibleModel isValueHolder.
	self assert: presenter specCompatibleModel == model.
	self assert: presenter specCompatibleModel announcer numberOfSubscriptions > 0.

