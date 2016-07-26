event handling
textChangedFrom: begin to: end delta: delta
	| changeInterval |
	self flag: 'Dirty code, that does not work well, to be reviewed'.
	((self interval rangeIncludes: begin) or: [ self interval rangeIncludes: end ])
		ifTrue: [ 
			self changeable
				ifTrue: [ ^ self delete ] ].
	changeInterval := begin to: end.
	((self interval rangeIncludes: end) and: [delta > 0])
		ifTrue: [ 
			self lastIndex: (self lastIndex + delta).
			^ self computeVertices ].
	((changeInterval rangeIncludes: begin) and: [ (changeInterval rangeIncludes: end) not ])
		ifTrue: [ 
			self firstIndex: self firstIndex - (self firstIndex - end).
			^ self computeVertices ].
	((changeInterval rangeIncludes: self firstIndex) and: [ changeInterval rangeIncludes: self lastIndex ])
		ifTrue: [ ^ self delete ].
	self textArea text size < self firstIndex
		ifTrue: [ ^ self delete ].
	self textArea text size < self lastIndex
		ifTrue: [ 
			self changeable
				ifTrue: [ ^ self delete ]
				ifFalse: [ 
					self lastIndex: self textArea text size.
					^ self computeVertices ] ].
	end < self firstIndex
		ifTrue: [ 
			firstIndex := self firstIndex + delta.
			lastIndex := self lastIndex + delta.
			^ self computeVertices ]