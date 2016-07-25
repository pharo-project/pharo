class
removeClasses: aCollection
	"Remove the selected classes from the system.  Check that the user really wants to do this, since it is not reversible.  Answer true if removal actually happened."

	| classNames classesToRemove result |
	aCollection isEmptyOrNil
		ifTrue: [ ^ false ].
	classesToRemove := aCollection collect: #theNonMetaClass.
	classNames := (classesToRemove collect: #name) joinUsing: ', '.
	(result := self confirm: (self removeClassesMessageFor: classNames))
		ifTrue: [ 
			classesToRemove
				do: [ :classToRemove | 
					classToRemove subclasses notEmpty
						ifTrue: [ 
							(self confirm: (self removedClassHasSubclassesMessageFor: classToRemove name))
								ifTrue: [ classToRemove removeFromSystem ] ]
						ifFalse: [ classToRemove removeFromSystem ] ] ].
	^ result