instance creation
separators
	"Answer a collection of the standard ASCII separator characters."

	^ #(32 "space"
		13 "cr"
		9 "tab"
		10 "line feed"
		12 "form feed")
		collect: [:v | Character value: v] as: String