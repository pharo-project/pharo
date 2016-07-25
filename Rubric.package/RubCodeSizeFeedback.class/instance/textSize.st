private
textSize
	| result count ignoredCharacters |
	result := 0.
	count := true.
	ignoredCharacters := self ignoredCharacters.
	self text string
		do: [ :char | 
			result > (self alertLimit * 3) 
				ifTrue: [ ^result ].
			(#($' $") includes: char)
				ifTrue: [ count := count not ]
				ifFalse: [ 
					((ignoredCharacters includes: char) not and: [ count ])
						ifTrue: [ result := result + 1 ] ] ].
	^ result