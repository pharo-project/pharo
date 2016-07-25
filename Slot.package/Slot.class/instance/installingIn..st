class building
installingIn: aClass
	"I am called by the class builder. This way a Slot can change the class it is installed in"
	self wantsInitalization ifTrue: [ self ensureSlotIniatializationFor: aClass ].