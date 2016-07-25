*Morphic-Widgets-Windows
activate
	"Mark the receiver and submorphs as active (foreground)."

	self submorphsDo: [:m | m activate]