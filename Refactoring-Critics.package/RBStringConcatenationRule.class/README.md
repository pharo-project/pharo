Check for string concatenation inside some iteration message. Since string concatenation is O(n^2), it is better to use streaming since it is O(n) - assuming that n is large enough. As a general principal avoid , since the receiver is copied. Therefore chaining , messages will lead to multiple useless copies of the receiver. 

Instead of writing
	| string | 
	string := String new.
	#(1 2 3) do: [ :each |
		string := string, each asString].
	^ string

Write, it is much more efficient.

	String streamContents: [:s | 
		#(1 2 3)  do: [:each | s nextPutAll: each asString]]
	
or more concisely...
	'''' join: {1. 1+1. 3}	
