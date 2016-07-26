testing
isLast: aNode 
	| last |
	statements isEmpty ifTrue: [^false].
	last := statements last.
	^last == aNode or: 
			[last isMessage and: 
					[(#(#ifTrue:ifFalse: #ifFalse:ifTrue:) includes: last selector) 
						and: [last arguments anySatisfy: [:each | each isLast: aNode]]]]