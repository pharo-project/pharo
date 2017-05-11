event handling
updatePattern: newPattern

	| searchPool t text |

	pattern = newPattern ifTrue: [ ^ self ].

	searchPool := pattern size > newPattern size 
		ifTrue: [ elements ]
		ifFalse: [ matchedElements ].

	t := [ matchedElements := self match: newPattern for: searchPool ] timeToRun.
	
	text := 'Checked {1} items, found {2} matching | {3} ms' format: { 
		elements size . matchedElements size. t asMilliSeconds }.
	
	stringMorph contents: text.		
	pattern := newPattern.	
	dataSource elements: matchedElements.	
	dataSource table ifNotNil: [ :fastTable | fastTable refresh ].
	