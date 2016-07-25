update
updateText
	"update the displayed list in a separate thread to avoid UI blocking"
	
	"if there is already a background thread running for the new list discard it"
	textRefreshingProcess ifNotNil: [
		textRefreshingProcess terminate].
	
	"fork off a possibly costly list calculation"
	textRefreshingProcess := [
		| indexes items |
		indexes := listModel selectedIndexes sort collect: [:i | i printString ].
		indexes :=  (indexes joinUsing: '; ').
		items := listModel selectedItems collect: [:i | i printString ].
		items := (items sort joinUsing: '; ').
		UIManager default defer: [
			textModel1 text: indexes.
			textModel2 text: items]] fork.