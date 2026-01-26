Extension { #name : 'FLMaterializer' }

{ #category : '*Tools' }
FLMaterializer class >> fileReaderServicesForFile: fullName suffix: suffix [
	<fileService>
	suffix = 'fuel'
		ifFalse: [ ^ #() ].
	^ { self serviceFuelMaterialize }
]

{ #category : '*Tools' }
FLMaterializer class >> inspectMaterializationFromFileNamed: aString [
	| result |
	result := self new
		filePath: aString;
		materializeRoot.
	^ result inspect
]

{ #category : '*Tools' }
FLMaterializer class >> serviceFuelMaterialize [
	^ FileServiceEntry 
		provider: self 
		label: 'Materialize Fuel file'
		selector: #inspectMaterializationFromFileNamed:
		description: 'Materialize objects previously serialized with Fuel'
		buttonLabel: 'materialize'
]
