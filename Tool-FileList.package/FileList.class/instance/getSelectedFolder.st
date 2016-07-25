private
getSelectedFolder
	"Answer the selected folder or nil if dialog was canceled"

	ok ifFalse: [ ^ nil ].
	^ self directory