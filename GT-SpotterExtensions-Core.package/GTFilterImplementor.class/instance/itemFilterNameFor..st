private-model
itemFilterNameFor: each
	self flag: 'this filter wants to filter items by #selector (speed 10x), but dive-in-category wants to filter by #printString. Most filters in dive-in-category wants to filter by #printString, but not all. Others: files, topics, help, ... ?'.
	^ each selector