brick-geometry
height: anObject
	<brickBuilderProperty: #Geometry name: 'Height' as: #asInteger getter: #height model: #GLMUIBuilderTextFieldProperty>

	self privateHeight: anObject.
	self updateHeight