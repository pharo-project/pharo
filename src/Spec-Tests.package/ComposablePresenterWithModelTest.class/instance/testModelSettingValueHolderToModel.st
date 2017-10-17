testing
testModelSettingValueHolderToModel

	"we had a Model, new model is a value holder"
	| model point valueHolder presenter |
	point := 1@0.
	model := TestingPointModel x: 1 y: 2.
	valueHolder := NewValueHolder value: point.
	presenter := TestingComposablePresenterWithModel on: model.
	self assert: presenter specCompatibleModel isSpecCompatibleModel.
	self deny: presenter specCompatibleModel isValueHolder.
	self assert: valueHolder isSpecCompatibleModel.
	self assert: valueHolder isValueHolder.
	presenter model: valueHolder.
	self assert: presenter specCompatibleModel isSpecCompatibleModel.
	self assert: presenter specCompatibleModel isValueHolder.
	self assert: presenter specCompatibleModel == valueHolder.
	self assert: presenter specCompatibleModel announcer numberOfSubscriptions > 0.
