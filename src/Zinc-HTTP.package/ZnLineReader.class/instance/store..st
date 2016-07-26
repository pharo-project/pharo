private
store: item
	position >= buffer size ifTrue: [ self growBuffer ].
	position := position + 1.
	buffer at: position put: item		
	