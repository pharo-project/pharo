accessing
currentWindow

	"Answer the top window or nil"
	
	^ self class environment at: #SystemWindow ifPresent: [:systemWindow | systemWindow topWindow]