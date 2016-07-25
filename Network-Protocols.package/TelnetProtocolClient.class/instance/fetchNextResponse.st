private protocol
fetchNextResponse
	"The FTP and similar protocols allow multi-line responses.
	If the response is multi-line, the fourth character of the first line is a  
	$- and the last line repeats the numeric code but the code is followed by 
	a space."

	| response result firstLine |
	result := '' writeStream.
	firstLine := self stream nextLine.
	result nextPutAll: firstLine.
	(self responseIsContinuation: firstLine) 
		ifTrue: 
			["continued over multiple lines. Discard continuation lines."

			
			[response := self stream nextLine.
			response ifNil: [^nil].
			response size > 3 and: 
					[(response copyFrom: 1 to: 3) = (firstLine copyFrom: 1 to: 3) 
						and: [(response at: 4) = Character space]]] 
					whileFalse: 
						[result
							cr;
							nextPutAll: response]].
	self lastResponse: result contents