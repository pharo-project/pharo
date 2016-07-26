private
handleStringDelimiter: char
	self escape
		ifTrue: [ escape := false ]
		ifFalse: [ 
			self insideString
				ifTrue: [ 
					char = delimiter
						ifTrue: [ delimiter := nil ] ]
				ifFalse: [ delimiter := char ] ].
	^ char