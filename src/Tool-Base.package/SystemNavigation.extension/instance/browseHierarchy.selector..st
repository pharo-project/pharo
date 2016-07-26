*Tool-Base
browseHierarchy: aClass selector: aSelector
	"Open a browser"

	aClass isNil
		ifTrue: [ ^ self ].
	^ Smalltalk tools browser new
		spawnHierarchyForClass: aClass selector: aSelector;
		yourself