drawing-support
preserveStateDuring: aBlock
	"Preserve the full canvas state during the execution of aBlock"
	^aBlock value: self copy