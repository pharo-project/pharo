testing
testModelSettingValueHolderToValueHolder

	| point point2 valueHolder presenter |
	
	"we had value holder, new model is a regular object"
	point := 40@54.
	point2 := 1@0.
	
	"we had value holder, new model is a value holder"
	presenter := TestingComposablePresenterWithModel on: point.
	valueHolder := NewValueHolder value: point2.
	self assert: presenter specCompatibleModel isSpecCompatibleModel.
	self assert: presenter specCompatibleModel isValueHolder.
	self deny: point2 isSpecCompatibleModel.
	self assert: valueHolder isValueHolder.
	presenter model: valueHolder.
	self assert: presenter specCompatibleModel isSpecCompatibleModel.
	self assert: presenter specCompatibleModel isValueHolder.
	self assert: presenter model == point2.
	self assert: presenter specCompatibleModel == valueHolder.
	self assert: presenter specCompatibleModel announcer numberOfSubscriptions > 0.
