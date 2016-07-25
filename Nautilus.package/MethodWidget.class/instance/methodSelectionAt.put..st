selection
methodSelectionAt: anIndex put: aBoolean
	| method |
	self flag: #clean.	"This method as classAt:put: and categoryAt:put: need to be clean latter. A class should not expose his implementation. Instead we should have selectMethod: unselectMethod:. Or better, this logic should be in AbstractWidget since all the subclasses remember the selected objects."
	method := self getMethods at: anIndex ifAbsent: [ ^ self ].
	aBoolean
		ifTrue: [ self selectMethod: method ]
		ifFalse: [ self removeSelectionOf: method ]