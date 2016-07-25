accessing
autoDeselect

	^ autoDeselect
		ifNil: [ self resetListSelector notNil ]
		ifNotNil: [ autoDeselect ]