brick-properties
rubricTextFamilyName

	^ self valueOfProperty: #brickRubricTextFamilyName ifAbsent: [ self themer rubricTextFamilyName ]