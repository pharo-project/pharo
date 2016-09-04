private
basicNext
	| nextChar |

	^ isBinary
		ifTrue: [super next]
		ifFalse: [
			(nextChar := super next)
				ifNotNil: [nextChar asCharacter]].