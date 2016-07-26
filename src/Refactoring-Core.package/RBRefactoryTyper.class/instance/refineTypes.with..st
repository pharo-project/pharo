computing types
refineTypes: aClassCollection with: anotherClassCollection 
	| classSet |
	classSet := Set new.
	aClassCollection do: 
			[:each | 
			anotherClassCollection do: 
					[:cls | 
					(cls includesClass: each) 
						ifTrue: [classSet add: cls]
						ifFalse: [(each includesClass: cls) ifTrue: [classSet add: each]]]].
	^classSet