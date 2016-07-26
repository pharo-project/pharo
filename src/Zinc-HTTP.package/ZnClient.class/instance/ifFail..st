options
ifFail: block
	"Set the block that I will be execute when the request fails.
	The exception of the failure will be passed as argument."
	
	self optionAt: #ifFailBlock put: block