accessing
sort: aBlock
	|aLinkedList|
	aLinkedList := LinkedList newFrom: (self asArray sort: aBlock).
	firstLink := aLinkedList firstLink.
	lastLink := aLinkedList lastLink.