converting
asString36
	"Encode the UUID as a base 36 string using 0-9 and lowercase a-z.
	This is the shortest representation still being able to work as
	filenames etc since it does not depend on case nor characters
	that might cause problems, and it fits into short filenames like on
	the old MacOS HFS filesystem. The check for 36r is to make this code
	work in versions before Squeak 3.8."

	| num candidate |
	num := 0.
	1 to: self size do: [:i | num := num + ((256 raisedTo: i - 1) * (self at: i))].
	candidate := num printStringBase: 36.
	^((candidate beginsWith: '36r')
			ifTrue: [candidate copyFrom: 4 to: candidate size]
			ifFalse: [candidate]) asLowercase