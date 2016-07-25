private
closeDelegate
	self delegate
		ifNotNil: [ :delegate | 
			(delegate respondsTo: #close)
				ifTrue: [ delegate close ] ]