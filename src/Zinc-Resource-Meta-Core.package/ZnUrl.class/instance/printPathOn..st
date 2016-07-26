printing
printPathOn: stream
	self hasPath ifFalse: [ ^ stream nextPut: $/ ].
 	segments do: [ :each | 
		stream nextPut: $/.
		each == #/
			ifFalse: [ 
				self encodePath: each on: stream ] ] 