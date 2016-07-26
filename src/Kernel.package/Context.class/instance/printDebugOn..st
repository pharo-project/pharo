printing
printDebugOn: aStream	
	"print a condensed for of the stack.
		For methods simply print Class >> selector
		For blocks only print the first line"
	| blockSource blockSourceSize |
	
	self printOn: aStream.
	self outerContext ifNil: [ ^ self ].
	"print the block..."
	aStream 
		nextPutAll: ' in Block: '.
	
	blockSource := closureOrNil printStringLimitedTo: 50.
	blockSourceSize := blockSource size.
	blockSource := blockSource copyUpTo: Character cr.
	
	aStream nextPutAll: blockSource.
	blockSource size < blockSourceSize
		ifTrue: [ aStream nextPutAll: '...' ].