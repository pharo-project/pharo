source
renameVariable: aString
	| class |
	class := self model selectedClass.
	(class instVarNames includes: aString)
		ifTrue: [ ^ self renameInstVarNamed: aString from: class ].
	(class classVarNames includes: aString)
		ifTrue: [ ^ self renameClassVarNamed: aString from: class ].
	self model flashSourceCodeArea