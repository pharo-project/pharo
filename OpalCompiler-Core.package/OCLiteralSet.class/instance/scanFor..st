private
scanFor: anObject
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| element start finish |
	start := (anObject hash \\ array size) + 1.
	finish := array size.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) isNil
					or: [element literalEqual: anObject])
					ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) isNil
					or: [element literalEqual: anObject])
					ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"