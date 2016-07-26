actions
browseInstVarRefs
	| cls |
	cls := self currentClassOrMetaClass.
	(cls notNil and: [cls isTrait not])
		ifTrue: [self systemNavigation browseInstVarRefs: cls]