accessing-path
path
	self hasPath ifFalse: [ ^ String new ].
	^ String streamContents: [ :stream | 
		segments 
			do: [ :each | 
				each == #/
					ifFalse: [ stream nextPutAll: each ] ]
			separatedBy: [ stream nextPut: $/ ] ]