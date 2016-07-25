collection ops
reject: aBlock
	"Implementation note: rejecting present is selecting absent"
	
	^(absent select: aBlock) complement