selecting
select: evt
	"Don't if not enabled."
	
	self isEnabled ifFalse: [^self].
	^super select: evt