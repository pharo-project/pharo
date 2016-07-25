private
loadFavoredEntries
	
	(narrowString notEmpty and: [narrowString first isUppercase]) 
			ifTrue: [ ^ classTable entriesMatching: narrowString ].
					
	includeSelectors 
		ifTrue: [ ^ table entriesMatching: narrowString ]
		ifFalse: [ ^ #() ].