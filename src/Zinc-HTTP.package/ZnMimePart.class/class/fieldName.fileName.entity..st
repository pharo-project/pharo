instance creation
fieldName: fieldName fileName: fileName entity: entity
	"Pathnames are often silenty encoded using UTF-8,
	this is a no-op for ASCII, but will fail on Latin-1 and others"

	| encodedFileName |
	encodedFileName := fileName utf8Encoded asString.
	^ self new
		setContentDisposition: 'form-data;name="', fieldName, '";filename="', encodedFileName, '"';
		entity: entity;
		yourself