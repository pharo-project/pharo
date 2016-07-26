accessing
currentWindow

	"Answer the top window or nil"
	
	^ #SystemWindow asClassIfPresent: [:systemWindow | systemWindow topWindow]