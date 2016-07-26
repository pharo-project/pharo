instance creation
newFrom: anAssociationCollection
	| newDictionary |

	newDictionary := self new: anAssociationCollection size.
	anAssociationCollection associationsDo: [:each |
		newDictionary
			at: each key
			put: each value].
	^ newDictionary.