private
scanForEmpty: anObject
	"Scan the key array for the first slot containing either a nil or an expired association (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found."
	| element start finish firstNil |
	finish := array size.
	start := self startIndexFor: anObject.
	firstNil := nil.
	
	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil 
						or: [self compare: element key to: anObject])
			ifTrue: [
				"If we found no match, and have an expired slot, use that one."
				(element isNil and: [firstNil notNil]) ifTrue:[^firstNil].
				^ index 
			].
			"If this slot is expired, and we haven't seen an earlier expired slot, record it"
			(element notNil 
				and: [element expired 
						and: [firstNil isNil]]) ifTrue: [firstNil := index].
		].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil 
					or: [self compare: element key to: anObject])
			ifTrue: [
				(element isNil and: [firstNil notNil]) ifTrue:[^firstNil].
				^ index 
			].
			(element notNil 
				and: [element expired 
						and: [firstNil isNil]]) ifTrue: [firstNil := index].
		].

	"If there is no empty slot, but an expired one, return it."	
	firstNil notNil ifTrue:[^firstNil].

	^ 0  "No match AND no empty slot"