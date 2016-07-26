private-primitives
class: aClass selector: aSelector method: aMethod pragma: aPragma
	^ GTExampleOrganizer instance atMethod: aMethod ifAbsentPut: [ self newExampleFromPragma: aPragma ]