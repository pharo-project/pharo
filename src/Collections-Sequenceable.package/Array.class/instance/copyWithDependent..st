copying
copyWithDependent: newElement
	self size = 0 ifTrue:[^DependentsArray with: newElement].
	^self copyWith: newElement