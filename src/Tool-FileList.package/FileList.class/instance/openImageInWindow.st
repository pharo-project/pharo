own services
openImageInWindow
	"Handle five file formats: GIF, JPG, PNG, Form stoteOn: (run coded), and
	BMP. Fail if file format is not recognized."
	| image |
	self reference streamWritable: false do: [ :stream|
		image := Form fromBinaryStream: stream].
	
	(World drawingClass withForm: image) openInWorld