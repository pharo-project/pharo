*Iceberg-UI
organizationDescription: tags 
	^ String streamContents: [ :stream |
		stream nextPutAll:	'Package tags: '.
		tags do: [ :elem | 
			stream nextPutAll: String crlf;
				nextPutAll: String tab;
				nextPutAll: elem.
	]]