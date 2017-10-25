options
ifFailBlock
	"Return the block that I should execute if the request fails.
	Defaults to #pass-ing the exception up. 
	Accepts an exception as argument."
	
	^ self optionAt: #ifFailBlock ifAbsentPut: [ [ :exception | exception pass ] ]