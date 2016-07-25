instance creation
fieldName: fieldName entity: entity
	^ self new
		setContentDisposition: 'form-data;name="', fieldName, '"';
		entity: entity;
		yourself