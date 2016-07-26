private
updateRemotes
	| roots |
	
	roots := tree roots 
		select: [ :e | e isPartialMatch ]
		thenCollect: [ :e | e content ].
		
	roots
		ifEmpty: [ self populateRemotes ]
		ifNotEmpty: [ self populateRemotesFor: roots ]