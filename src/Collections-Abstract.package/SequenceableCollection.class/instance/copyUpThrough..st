copying
copyUpThrough: anElement
    "Answer all elements up to and including anObject. If there
     is no such object, answer a copy of the receiver."

	^self first: (self indexOf: anElement ifAbsent: [^ self copy])