*Graphics-Files-FileRegistry
openImageInWindow: fullName 
	"Handle five file formats: GIF, JPG, PNG, Form storeOn: (run coded), and
	BMP. Fail if file format is not recognized."
	
	| image |
	image := self fromFileNamed: fullName.
	(World drawingClass withForm: image) openInWorld