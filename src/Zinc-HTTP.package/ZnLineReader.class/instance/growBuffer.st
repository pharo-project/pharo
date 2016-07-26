private
growBuffer
	| newBuffer |
	buffer size >= limit 
		ifTrue: [
			(ZnLineTooLong limit: limit) signal ].
	newBuffer := String new: buffer size * 2.
	newBuffer replaceFrom: 1 to: buffer size with: buffer.
	buffer := newBuffer 