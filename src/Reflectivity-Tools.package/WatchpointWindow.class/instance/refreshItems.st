updating
refreshItems
	| max values |
	values := self watchpoint values.
	max := values size.
	list items: (values copyFrom: (1 max: max - numItems) to: max) reversed. 