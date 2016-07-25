*GT-InspectorExtensions-Core
gtInspectorFuelIn: composite
	<gtInspectorPresentationOrder: 40>
	"(FileSystem disk workingDirectory / 'GTtest.fuel') writeStreamDo: [ :stream |
	FLSerializer newDefault
		at: #author putAdditionalObject: 'Author: Max Leske';
		at: #date putAdditionalObject: DateAndTime now;
		addPreMaterializationAction: [ Smalltalk inform: 'starting materialization' ];
		addPostMaterializationAction: [ :materialization | materialization inspect ];
		serialize: 'foo' on: stream binary ].
	
FLMaterializer materializeFromFileNamed: 'GTtest.fuel'"
	composite table
			title: 'Fuel Header';
			display: [
				(FLMaterializer materializeHeaderFromFileNamed: self fullName) gtGetMetaData ];
			column: 'Property' evaluated: [:assoc| assoc key ];
			column: 'Value' evaluated: [:assoc| assoc value];
			when: [ self isFile and: [self extension = 'fuel' ] ]