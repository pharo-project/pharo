instance creation
newInstanceFrom: oldInstance variable: variable size: instSize map: map
	"Create a new instance of the receiver based on the given old instance.
	The supplied map contains a mapping of the old instVar names into
	the receiver's instVars"
	| new |
	new := self newFrom: oldInstance.
	1 to: instSize do: 
		[:offset |  (map at: offset) > 0 ifTrue:
			[new instVarAt: offset
					put: (oldInstance instVarAt: (map at: offset))]].
	^new