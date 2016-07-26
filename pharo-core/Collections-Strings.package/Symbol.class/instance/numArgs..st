system primitives
numArgs: n
	"Answer a string that can be used as a selector with n arguments.
	 TODO: need to be extended to support shrinking and for selectors like #+ " 

	| selector numArgs offs |
	
	selector := self.
	(numArgs := selector numArgs) >= n 
		ifTrue: [ ^ self ].
	
	^ self class new: 16 streamContents: [ :stream|
		stream nextPutAll: self.
		(numArgs = 0) 
			ifTrue: [ stream nextPut: $:. offs := 0] 
			ifFalse: [ offs := 1 ].
		2 to: n - numArgs + offs do: [:i | 
			stream nextPutAll: 'with:' ]].
	
