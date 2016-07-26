actions browsing
browseInstVarRefs
	"1/26/96 sw: real work moved to class, so it can be shared"
	| cls |
	cls := self currentClassOrMetaClass.
	(cls notNil and: [cls isTrait not])
		ifTrue: [self systemNavigation browseInstVarRefs: cls]