private
unexpectedIdentityEqualityStringBetween: actual and: expected

	^ String streamContents: [:stream |
			stream
				nextPutAll: 'Unexpected identity equality of ';
				nextPutAll: actual fullPrintString;
				nextPutAll: ' and ';
				nextPutAll: expected fullPrintString;
				nextPutAll: '.']