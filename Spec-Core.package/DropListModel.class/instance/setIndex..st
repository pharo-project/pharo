api
setIndex: anIndex
	| item |

	selectionHolder index value: anIndex.
	item := (listHolder value at: anIndex ifAbsent: [ nil ]).
	item value.
	selectionHolder selection value: item.
	self changed: #getIndex