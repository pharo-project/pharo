*GT-InspectorExtensions-Core
gtInspectorTempNodes

	^ (self tempNames withIndexCollect: [ :tempName :tempIndex |
			GTInspectorContextNamedTempNode hostObject: self tempName: tempName tempIndex: tempIndex ]) .