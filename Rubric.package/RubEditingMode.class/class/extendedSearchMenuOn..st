menu
extendedSearchMenuOn: aBuilder
	(aBuilder item: #'extendedSearch' translated)
		label: 'Code search...' translated;
		withSeparatorAfter;
		with: [ 
					(aBuilder item: #'browse it' translated)
						keyText: 'b';
						selector: #browseIt.
					(aBuilder item: #'senders of it' translated)
						keyText: 'n';
						selector: #sendersOfIt.
					(aBuilder item: #'implementors of it' translated)
						keyText: 'm';
						selector: #implementorsOfIt.
					(aBuilder item: #'references to it' translated)
						keyText: 'N';
						selector: #referencesToIt;
						withSeparatorAfter.
					(aBuilder item: #'selectors containing it' translated)
						keyText: 'W';
						selector: #methodNamesContainingIt.
					(aBuilder item: #'case insensitive method literal strings with it' translated)
						keyText: 'E';
						selector: #methodStringsContainingit.
					(aBuilder item: #'case sensitive method literal strings with it' translated)
						keyText: 'E';
						selector: #methodCaseSensitiveStringsContainingit.
					(aBuilder item: #'method source with it' translated)
						selector: #methodSourceContainingIt;
						withSeparatorAfter.
					(aBuilder item: #'class names containing it' translated) selector: #classNamesContainingIt.
					(aBuilder item: #'class comments with it' translated) selector: #classCommentsContainingIt ]