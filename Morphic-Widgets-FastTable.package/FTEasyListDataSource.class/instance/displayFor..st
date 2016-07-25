private
displayFor: rowIndex
	| element |
	element := self elementAt: rowIndex.
	^ displayBlock 
		ifNotNil: [ displayBlock value: element ]
		ifNil: [ ^ element ].