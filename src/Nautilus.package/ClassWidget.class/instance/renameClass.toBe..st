protocol
renameClass: aClass toBe: aString
	| newName oldName obs |
	aString isEmptyOrNil ifTrue: [ ^ self ].	"Cancel returns"
	newName := aString asSymbol.
	newName = aClass name asSymbol ifTrue: [ ^ self ].
	oldName := aClass name asSymbol.
	(aClass environment includesKey: newName)
		ifTrue: [ ^ self inform: newName , ' already exists' ].
	aClass rename: newName.

	obs := self model systemNavigation allReferencesTo: (aClass environment associationAt: newName).
	obs isEmpty
		ifFalse: [ self model systemNavigation browseMessageList: obs name: 'Obsolete References to ' , oldName autoSelect: oldName ].
	^ newName