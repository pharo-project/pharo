process specific
resetPSValueAt: index

	"NOTE: this method are PRIVATE. "

	env ifNil: [ ^ self ]. 
	index > env size ifTrue: [ ^ self ].
	
	env at: index put: nil