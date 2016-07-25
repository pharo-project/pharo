displaying
displayString
	^ String streamContents: [ :stream |
		stream
			nextPutAll: 'A cycle of ';
			print: self size;
			nextPutAll: ' packages : '.
		packages
			do: [ :package | stream nextPutAll: package packageName ]
			separatedBy: [ stream space ] ]