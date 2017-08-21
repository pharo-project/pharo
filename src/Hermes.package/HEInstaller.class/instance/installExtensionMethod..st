installing methods
installExtensionMethod: extensionMethod
	| aClass |
	aClass := self environment classNamed: extensionMethod className.
	self rebuildMethod: extensionMethod into: aClass.