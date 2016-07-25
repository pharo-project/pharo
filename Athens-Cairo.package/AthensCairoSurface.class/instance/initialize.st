initialize-release
initialize
	"the handle should be set already since we using an NB callout to create an instance"
	handle value = 0 ifTrue: [ 
		self error: 'Error creating new surface' ].
	
	session := Smalltalk session.
	
	id := self class registerSurface: self.
		
	context := self newCanvas.
	builder := AthensCairoPathBuilder new.
	builder context: context.
	ftFontRenderer := CairoFreetypeFontRenderer new
		canvas: context. 
	
	FFIExternalResourceManager addResource: self.