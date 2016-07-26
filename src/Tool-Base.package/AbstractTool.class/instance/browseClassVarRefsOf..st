class
browseClassVarRefsOf: aClass

	| cls |
	cls := aClass theNonMetaClass.
	(cls notNil and: [cls isTrait not])
		ifTrue: [self systemNavigation browseClassVarRefs: cls]