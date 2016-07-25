drag and drop
dropInAClass: aCollection into: aClass

	aCollection do: [:anObject | 
		(anObject isBehavior)
			ifFalse: [
				(anObject isString or: [ anObject isSymbol ])
					ifTrue: ["category"
						ActiveHand shiftPressed
							ifTrue: [ self copyCategory: anObject toTheClass: aClass ]
							ifFalse: [ self moveCategory: anObject toTheClass: aClass ]]
					ifFalse: [
						"a method"
						ActiveHand shiftPressed
							ifTrue: [ self copyMethod: anObject toTheClass: aClass ]
							ifFalse: [ self moveMethod: anObject toTheClass: aClass autoRemove: false ]]]]