*Ring-Core-Kernel
asRingDefinition
	"Retrieves a RGMethodDefinition object based on the data of the receiver"
	
	self type == #method
		ifTrue: [ ^self createMethodDefinition ].
	self type == #classComment
		ifTrue: [ ^self createCommentDefinition ].
	^nil