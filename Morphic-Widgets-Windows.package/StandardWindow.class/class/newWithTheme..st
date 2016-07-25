instance creation
newWithTheme: aUITheme
	"Answer a new window with the given theme."

	^ self basicNew
		basicTheme: aUITheme;
		initialize