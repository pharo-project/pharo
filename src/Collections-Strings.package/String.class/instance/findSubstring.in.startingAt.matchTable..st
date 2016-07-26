system primitives
findSubstring: key in: body startingAt: start matchTable: matchTable
	"Answer the index in the string body at which the substring key first occurs, at or beyond start.  The match is determined using matchTable, which can be used to effect, eg, case-insensitive matches.  If no match is found, zero will be returned."
	| index c1 c2 |
	matchTable == nil ifTrue: [
		key size = 0 ifTrue: [^ 0].
		start to: body size - key size + 1 do:
			[:startIndex |
			index := 1.
				[(body at: startIndex+index-1)
					= (key at: index)]
					whileTrue:
					[index = key size ifTrue: [^ startIndex].
					index := index+1]].
		^ 0
	].

	key size = 0 ifTrue: [^ 0].
	start to: body size - key size + 1 do:
		[:startIndex |
		index := 1.
		[c1 := body at: startIndex+index-1.
		c2 := key at: index.
		((c1 leadingChar = 0 and: [ c1 asciiValue < matchTable size ]) 
			ifTrue: [ matchTable at: c1 asciiValue + 1 ]
			ifFalse: [ c1 asciiValue + 1 ]) = 
			((c2 leadingChar = 0 and: [ c2 asciiValue < matchTable size ])
				ifTrue: [ matchTable at: c2 asciiValue + 1 ]
				ifFalse: [c2 asciiValue + 1 ]) ]
			whileTrue:
				[index = key size ifTrue: [^ startIndex].
				index := index+1]].
	^ 0