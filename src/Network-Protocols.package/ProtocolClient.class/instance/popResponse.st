private
popResponse
	| pendingResponse |
	pendingResponse := self pendingResponses removeFirst.
	pendingResponses isEmpty
		ifTrue: [pendingResponses := nil].
	^pendingResponse