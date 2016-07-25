tests
testCreatingWindow

	| window attributes |
	attributes := OSWindowAttributes new.
	attributes preferableDriver: OSNullWindowDriver new.
	window := OSWindow createWithAttributes: attributes.

	self assert: (window isValid).
