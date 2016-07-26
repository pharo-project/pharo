class
renameClass: aClass
	| oldName newName obs |

	aClass ifNil: [ ^ self ].
	
	oldName := aClass name.
	newName := UIManager default  request: 'Please type new class name' initialAnswer: oldName.
	newName isEmptyOrNil ifTrue: [ ^ self ].	"Cancel returns"
	newName := newName asSymbol.
	newName = oldName ifTrue: [ ^ self ].
	(self class environment includesKey: newName)
		ifTrue: [ ^ self error: newName , ' already exists' ].
	aClass rename: newName.

	obs := self systemNavigation allReferencesTo: (aClass environment associationAt: newName).
	obs isEmpty
		ifFalse: [ self systemNavigation browseMessageList: obs name: 'Obsolete References to ' , oldName autoSelect: oldName ].
	^ newName