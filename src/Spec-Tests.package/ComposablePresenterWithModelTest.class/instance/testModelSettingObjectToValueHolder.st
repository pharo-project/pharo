testing
testModelSettingObjectToValueHolder

	| point point2 presenter |
	
	"we had value holder, new model is a regular object"
	point := 40@54.
	point2 := 1@0.
	presenter := TestingComposablePresenterWithModel on: point.
	self assert: presenter specCompatibleModel isSpecCompatibleModel.
	self assert: presenter specCompatibleModel isValueHolder.
	self deny: point2 isSpecCompatibleModel.
	self deny: point2 isValueHolder.
	presenter model: point2.
	self assert: presenter specCompatibleModel isSpecCompatibleModel.
	self assert: presenter specCompatibleModel isValueHolder.
	self assert: presenter model == point2.
	self assert: presenter specCompatibleModel announcer numberOfSubscriptions > 0.
