*Morphic-Widgets-Windows
nextVisibleWindow
	"Answer the next (visible) window to navigate to."

	^self visibleSystemWindows 
		ifEmpty: [ nil ] 
		ifNotEmpty: [ :windows | 
			windows after: self currentWindow ifAbsent: [windows first]
		]
