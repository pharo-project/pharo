fileIn/Out
basicChunk
	"If our buffer in collection contains an chunk with no embedded !'s, nor
	any non-ascii characters, return that.
	This presumes the source code encoding is unambiguously ascii-compatible"
	| bufferIX goodString |
	"Not possible if read buffering disabled"
	collection ifNil: [^nil].
	^ ((bufferIX := (collection indexOf: $! startingAt: position + 1) min: readLimit +1) > 0
			and: [bufferIX < collection size
					and: [(collection at: bufferIX + 1)
								~= $!
							and: [goodString := collection copyFrom: position + 1 to: bufferIX - 1.
								goodString isAsciiString]]])
		ifTrue: [ position := bufferIX.
				goodString]