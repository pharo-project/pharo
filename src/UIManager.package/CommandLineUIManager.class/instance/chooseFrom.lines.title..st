ui requests
chooseFrom: aList lines: linesArray title: aString
	| maxPad |
	maxPad := aList size asString size.
	self stdout 
		nextPutAll: aString; nextPut: $:; cr.
	aList withIndexDo: [ :item :index |
		self stdout 
			nextPutAll: '    ['; nextPutAll: (index asString padLeftTo: maxPad); nextPutAll: '] ';
			print: item; cr].
	self stdout nextPutAll: '> '.	
	^ aList at: (self stdin upToAnyOf: String crlf do: [ :chr| ]) asInteger.