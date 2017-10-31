testing
testModelSettingObjectToValueHolder

	| point point2 presenter |
	
	"we had value holder, new model is a regular object"
	point := 40@54.
	point2 := 1@0.
	presenter := TestingComposablePresenterWithModel on: point.
	self assert: presenter announcingObject isAnnouncingObject.
	self assert: presenter announcingObject isValueHolder.
	self deny: point2 isAnnouncingObject.
	self deny: point2 isValueHolder.
	presenter model: point2.
	self assert: presenter announcingObject isAnnouncingObject.
	self assert: presenter announcingObject isValueHolder.
	self assert: presenter model == point2.
	self assert: presenter announcingObject announcer numberOfSubscriptions > 0.
