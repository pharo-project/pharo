event handling
searchStringChanged: newString

	| searchPool t |

	pattern = newString ifTrue: [ ^ self ].

	searchPool := 
		pattern size > newString size 
			ifTrue: [ elements ]
			ifFalse: [ matchedElements ].

	t := [ matchedElements := self match: newString for: searchPool ] timeToRun.
	
	stringMorph contents: (String streamContents: [ :str |
		str 
		nextPutAll: 'found ';
		print: matchedElements size; 
		nextPutAll: ' of ';
		print: searchPool size;
		nextPutAll: ' elements | '; 
		print: t ]
	).
		
	pattern := newString.
	
	dataSource elements: matchedElements; tableRefresh