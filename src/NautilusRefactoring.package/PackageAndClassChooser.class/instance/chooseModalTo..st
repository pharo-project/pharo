actions
chooseModalTo: parentWindow
	"Open this this package class chooser as a dialog modal to the given window"
	| dialog result |
	
	dialog := self openDialogWithSpec.
	result := nil.
	dialog 
		okAction: [ result := self selectedItems ];
		cancelAction: [ result := nil ].
	parentWindow openModal: dialog window.
	^ result