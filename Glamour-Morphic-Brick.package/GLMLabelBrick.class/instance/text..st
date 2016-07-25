brick-properties
text: aString
	<brickBuilderProperty: #Text name: 'Text' as: #asString getter: #text model: #GLMUIBuilderTextFieldProperty>
	
	self label text: aString.
	self wrappedBounds markWidthDirty.
	self doLayout