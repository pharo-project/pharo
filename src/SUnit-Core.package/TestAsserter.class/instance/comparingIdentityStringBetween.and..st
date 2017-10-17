private
comparingIdentityStringBetween: actual and: expected

	^ String streamContents: [:stream |
			stream
				nextPutAll: actual fullPrintString;
				nextPutAll: ' is not identical to ';
				nextPutAll: expected fullPrintString;
				nextPutAll: '.']