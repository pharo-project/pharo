tests
testMethodAdditionWithProtocolChanged

	| aClass anEntry |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^42' classified: 'number'.
	anEntry := monitor log head.

	(aClass>>#fortyTwo) protocol: 'none'.
	
	self assert: (aClass>>#fortyTwo) protocol equals: 'none'.
	self apply: { anEntry }.
	self assert: (aClass>>#fortyTwo) protocol equals: 'number'.
