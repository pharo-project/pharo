stack list
pcRange
	"Answer the indices in the source code for the method corresponding to  
	the selected context's program counter value."
	(selectedContext isNil or: [methodText isEmptyOrNil])
		ifTrue: [^ 1 to: 0].
	^selectedContext debuggerMap
		rangeForPC: (selectedContext pc ifNotNil: [:pc| pc] ifNil: [selectedContext method endPC])
		contextIsActiveContext: stackListIndex = 1