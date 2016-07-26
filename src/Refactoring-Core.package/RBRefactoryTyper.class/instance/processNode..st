equivalence classes
processNode: aNode 
	(aNode isVariable and: [class instanceVariableNames includes: aNode name]) 
		ifTrue: [^self merge: aNode name].
	(aNode isMessage 
		and: [aNode receiver isVariable and: [aNode receiver name = 'self']]) 
			ifTrue: [^self merge: aNode selector].
	aNode isAssignment 
		ifTrue: 
			[self
				processNode: aNode value;
				processNode: aNode variable].
	(aNode isMessage and: 
			[#(#ifTrue: #ifFalse: #ifTrue:ifFalse: #ifFalse:ifTrue:) 
				includes: aNode selector]) 
		ifTrue: 
			[aNode arguments do: 
					[:each | 
					each isBlock 
						ifTrue: 
							[each body statements isEmpty 
								ifFalse: [self processNode: each body statements last]]]]