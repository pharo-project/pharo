menu declaration
smalltalkEditorShiftedMenuOn: aBuilder 
	"Specify the menu used when writing code. Try it with:
	(PragmaMenuBuilder 
		pragmaKeyword: 'smalltalkEditorShiftedMenu'
		model: nil) menu popUpInWorld"

	<contextMenu>
	<smalltalkEditorShiftedMenu>

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
	(aBuilder item: #'method literal strings with it' translated) 
		keyText: 'E';
		selector: #methodStringsContainingit.
	(aBuilder item: #'method source with it' translated) 
		selector: #methodSourceContainingIt; 
		withSeparatorAfter.
	(aBuilder item: #'class names containing it' translated) 
		selector: #classNamesContainingIt.
	(aBuilder item: #'class comments with it' translated) 
		selector: #classCommentsContainingIt.
