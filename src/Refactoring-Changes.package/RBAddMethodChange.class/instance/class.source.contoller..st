initialize-release
class: aClass source: aString contoller: aController
	self changeClass: aClass.
	source := aString.
	self protocols: (RBBrowserEnvironment new whichProtocolIncludes: self selector in: aClass).
	controller := aController