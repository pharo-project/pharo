accessing
stream

	^ stream ifNil: [
		stream := self defaultStream.
		stream
			onAddedSelector: #addCandidate:in:;
			onRemoveSelector: #removeAllCandidatesOf:;
			receiver: self candidates.
		stream ]