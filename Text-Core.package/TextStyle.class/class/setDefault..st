constants
setDefault: aTextStyle
	"Answer the system default text style."

	TextSharedInformation at: #DefaultTextStyle put: aTextStyle.