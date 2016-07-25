private
ensureSlotIniatializationFor: aClass
	self ensureInitalizeMethodExists: aClass. 
	self addSlotInitToInitialize: aClass.