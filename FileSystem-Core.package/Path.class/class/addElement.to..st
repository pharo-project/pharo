private
addElement: element to: result
	element = '..'
		ifTrue: [ ^ self addParentElementTo: result ].
	element = ''
		ifTrue: [ ^ self addEmptyElementTo: result ].
	element = '.'
		ifFalse: [ result add: element ]