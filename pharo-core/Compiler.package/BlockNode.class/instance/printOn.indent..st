printing
printOn: aStream indent: level

	"statements size <= 1 ifFalse: [aStream crtab: level]."
	aStream nextPut: $[.
	self printArgumentsOn: aStream indent: level.
	(self printTemporaries: temporaries on: aStream doPrior: []) ifTrue:
		["If >0 temps and >1 statement, put all statements on separate lines"
		 statements size > 1
			ifTrue: [aStream crtab: level]
			ifFalse: [aStream space]].
	self printStatementsOn: aStream indent: level.
	aStream nextPut: $]