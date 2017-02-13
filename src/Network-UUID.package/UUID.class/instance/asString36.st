converting
asString36
	"Return a String encoding the receiver as a base 36 number using 0-9 and lowercase a-z.
	This is the shortest representation still being able to work as
	filenames etc since it does not depend on case nor characters
	that might cause problems, and it is reasonably short."

	| num |
	num := 0.
	1 to: self size do: [ :i | num := num + ((256 raisedTo: i - 1) * (self at: i)) ].
	^ (num printStringBase: 36) asLowercase