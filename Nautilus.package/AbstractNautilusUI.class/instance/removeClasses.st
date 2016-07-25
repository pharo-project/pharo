menus behavior
removeClasses
	"Remove the selected classes from the system, at interactive user request.  Make sure the user really wants to do this, since it is not reversible.  Answer true if removal actually happened."

	| result scroll |
	self okToChange ifFalse: [^ false].
	scroll := self classWidget vScrollValue.
	result := self removeClasses: (self selectedClasses collect: #theNonMetaClass).

	result
		ifTrue: [	
			self resetClassesListSelection.
			self selectedClass: nil ].
	self classWidget vScrollValue: scroll.
	^ result