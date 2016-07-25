source code management
setSourcePointer: srcPointer
	"We can't change the trailer of existing method, since it could have completely different format. 	
	Therefore we need to generate a copy with new trailer, containing scrPointer, and then become it."

	| trailer copy |
	trailer := CompiledMethodTrailer new sourcePointer: srcPointer.
	copy := self copyWithTrailerBytes: trailer.
	"If possible do a replace in place as an optimization"
	(self trailer class == trailer class and: [ self size = copy size ])
		ifTrue: [ 
			| start |
			start := self endPC + 1.
			self replaceFrom: start to: self size with: copy startingAt: start ]
		ifFalse: [ self becomeForward: copy ].
	^ self
