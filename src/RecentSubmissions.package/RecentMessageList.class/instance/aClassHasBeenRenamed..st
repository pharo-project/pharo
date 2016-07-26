event handler
aClassHasBeenRenamed: anEvent

	| toChangeList |
	toChangeList := self methodReferenceList select: [:each | each className = anEvent oldName asSymbol].
	toChangeList do: [:each | each parentName: anEvent newName asSymbol].
	self updateView.