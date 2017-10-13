private
clickOnSelectedItem
	| item |
	
	item := self selectedItem.
	(item notNil and: [ item respondsTo: #click ])
		ifTrue: [ ^ item click ]
		ifFalse: [ ^ false ]