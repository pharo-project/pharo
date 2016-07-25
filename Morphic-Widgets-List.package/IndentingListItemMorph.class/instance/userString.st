accessing
userString
	"Add leading tabs to my userString"
	^ (String new: indentLevel withAll: Character tab), super userString
