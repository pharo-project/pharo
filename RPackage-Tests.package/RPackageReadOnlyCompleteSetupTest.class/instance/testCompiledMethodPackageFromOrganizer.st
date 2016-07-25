test compiled method
testCompiledMethodPackageFromOrganizer
	"self debug: #testCompiledMethodPackageFromOrganizer"
	| method |
	method := (Smalltalk at: #A1DefinedInP1) >> #methodDefinedInP1.
	self assert: (method packageFromOrganizer: self packageClass organizer) = p1.
	method := (Smalltalk at: #A2DefinedInP2) >> #methodDefinedInP1.
	self assert: (method packageFromOrganizer: self packageClass organizer) = p1