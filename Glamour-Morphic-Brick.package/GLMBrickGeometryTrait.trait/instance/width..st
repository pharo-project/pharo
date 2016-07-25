brick-geometry
width: anObject
	<brickBuilderProperty: #Geometry name: 'Width' as: #asInteger getter: #width model: #GLMUIBuilderTextFieldProperty>

	self privateWidth: anObject.
	self updateWidth