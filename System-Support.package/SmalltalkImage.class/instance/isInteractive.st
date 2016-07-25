testing
isInteractive
	"Check if vm were run with headless parameter.
	Different VMs for different platform have different multiple way(s) to indicate that"
	
	"non-headless mode is always interactive"
	self isHeadless ifFalse: [ ^ true ].
	
	-1000 to: 1000 do: [ :n | 
		(#('-interactive') includes: (self vm getSystemAttribute: n)) 
			ifTrue: [ ^ true ]].
	
	^ false