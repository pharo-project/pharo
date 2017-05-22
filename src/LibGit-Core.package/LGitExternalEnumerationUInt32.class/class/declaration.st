accessing
declaration
	^ declaration ifNil: [ declaration := self literalDeclaration ]