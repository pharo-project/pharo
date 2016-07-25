private
hasCreatedClassFor: aBehavior 
	| className |
	className := self classNameFor: aBehavior.
	^(newClasses includesKey: className) 
		or: [changedClasses includesKey: className]