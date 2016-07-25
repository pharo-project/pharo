actions
browseClassVarRefs
	| cls |
	cls := self currentClass.
	(cls notNil and: [cls isTrait not])
		ifTrue: [self systemNavigation  browseClassVarRefs: cls]