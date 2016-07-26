accessing-packages
packageAt: aName ifAbsent: absentBlock
	| package |
	package := self packageOrganizer 
		packageNamed: aName ifAbsent: absentBlock.
	
	^ (self includesCategory: aName)
		ifTrue: [ package ]
		ifFalse: absentBlock