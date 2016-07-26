api
updateTitle
	"Update the window title"

	self window ifNotNil: [:w | w updateTitle ]