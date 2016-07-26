accessing
autoDeselect
	^ self resetListSelector 
		ifNotNil: [ true ]
		ifNil: [ self allowsDeselection ]
		