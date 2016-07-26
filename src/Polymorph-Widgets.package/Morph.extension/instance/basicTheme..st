*Polymorph-Widgets
basicTheme: aUITheme
	"Set the current theme for the receiver."

	self theme = aUITheme ifFalse: [
		self setProperty: #theme toValue: aUITheme]