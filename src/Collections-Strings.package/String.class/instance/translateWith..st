converting
translateWith: table
	"translate the characters in the string by the given table, in place"
	^ self translateFrom: 1 to: self size table: table