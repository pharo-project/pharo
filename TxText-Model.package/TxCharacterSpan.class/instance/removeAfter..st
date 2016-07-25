operation
removeAfter: anOffset
	characters := characters first: anOffset.
	characters isEmpty ifTrue: [ self delete ]