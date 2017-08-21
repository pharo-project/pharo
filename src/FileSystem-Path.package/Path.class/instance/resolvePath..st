navigating
resolvePath: aPath
	"Answers a path created by resolving the argument against the receiver.
	If the argument is abolute answer the argument itself. Otherwise, concatenate the
	two paths."

	| elements |

	aPath isAbsolute ifTrue: [^ aPath].
	elements := Array new: self size + aPath size.

	1 to: self size do: [:i | elements at: i put: (self at: i)].
	1 to: aPath size do: [:i | elements at: self size + i put: (aPath at: i)].
	
	^ self class withAll: elements