instance creation
openOnHostNamed: hostName
	"If the hostname uses the colon syntax to express a certain portnumber
	we use that instead of the default port number."

	| i |
	i := hostName indexOf: $:.
	i = 0 ifTrue: [
			^self openOnHostNamed: hostName port: self defaultPortNumber]
		ifFalse: [
			| s p | 
			s := hostName truncateTo: i - 1.
			p := (hostName copyFrom: i + 1 to: hostName size) asInteger.
			^self openOnHostNamed: s port: p]
	