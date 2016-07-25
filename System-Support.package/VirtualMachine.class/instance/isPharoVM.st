testing
isPharoVM
	"Answers if this VM is a valid PharoVM (made with our sources)"
	| version |
	
	self flag: #todo. "We need a certain way of identify PharoVM, but in the mean time this ensures "
	version := self version.
	^ (version beginsWith: 'PharoVM')
		or: [ 
			(version includesSubstring: 'NBCoInterpreter')
				or: [
					(version includesSubstring: 'Commit: ') 
						and: [ 
							| pos |
							pos := (version findString: 'Commit: ') + 8.
							(version copyFrom: pos to: pos + 39) allSatisfy: [ :each | (($a to: $f), ($0 to: $9)) includes: each ] ] ] ]