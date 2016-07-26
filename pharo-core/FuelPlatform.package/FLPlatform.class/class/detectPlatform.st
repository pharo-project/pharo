private
detectPlatform
	| matches mostSpecific |
	matches := self allSubclasses 
		select: [ :class | class isResponsibleForCurrentPlatform ].
	mostSpecific := matches first.
	matches do: [ :class |
		class allSubclasses size < mostSpecific allSubclasses size ifTrue: [
			mostSpecific := class ] ].
	
	^ mostSpecific new