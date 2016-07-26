accessing
size
	"normally a slot takes one ivar. Virtual slots do not take space. 
	 We could even have Slots that map to multiple ivars"
	^self isVirtual ifTrue: [0] ifFalse: [1]