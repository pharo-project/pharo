private
indexForItemName: aString andQuery: aQueryString
	^ aQueryString isEmpty
		ifTrue: [ 1 ] " if the modified query is empty, take all items -> index = 1 "
		ifFalse: [ super indexForItemName: aString andQuery: aQueryString ]