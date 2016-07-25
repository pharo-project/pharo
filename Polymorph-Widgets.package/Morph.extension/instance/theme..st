*Polymorph-Widgets
theme: aUITheme
	"Set the current theme for the receiver."

	self theme = aUITheme ifFalse: [
		self basicTheme: aUITheme.
		self themeChanged]