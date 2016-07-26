instance creation
className: aString selector: aSelector isMetaSide: aBoolean

	^ (self class: (RGClassDefinition named: aString) selector: aSelector) isMetaSide: aBoolean; yourself