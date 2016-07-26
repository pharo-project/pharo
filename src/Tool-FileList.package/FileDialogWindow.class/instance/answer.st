accessing
answer
	"Answer the result of performing the action selector."
	
	self cancelled ifTrue: [^nil].
	^answer