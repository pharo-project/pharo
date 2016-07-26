accessing
match: subCollection
	"Try to read and match the elements of subCollection. 
	If successful return true and leave the me positioned after the match.
	If unsuccesful return false and leave me at end."
	
	| buffer pattern bufferPosition bufferLimit |
	pattern := subCollection readStream.
	"we have to use an internal buffer because we are not positionable"
	buffer := subCollection species new: subCollection size.
	bufferPosition := bufferLimit := 0.
	[ pattern atEnd ] whileFalse: [ | nextElement |
		self atEnd ifTrue: [ ^ false ].
		"get the next char to match either from the buffer or from the stream while buffering it"
		(bufferPosition < bufferLimit) 
			ifTrue: [ 
				nextElement := buffer at: bufferPosition + 1 ]
			ifFalse: [
				nextElement := buffer at: bufferPosition + 1 put: self next.
				bufferLimit := bufferLimit + 1 ].
		bufferPosition := bufferPosition + 1.
		pattern next = nextElement
			ifFalse: [
				"shift the buffer down one element and restart"
				2 to: bufferLimit do: [ :each |
					buffer at: each - 1 put: (buffer at: each) ].
				bufferLimit := bufferLimit - 1.
				bufferPosition := 0.
				pattern position: 0 ] ].
	^ true