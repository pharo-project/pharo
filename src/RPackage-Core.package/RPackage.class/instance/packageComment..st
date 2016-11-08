accessing
packageComment: aDescription
	^ self packageManifest theMetaClass
		ifNil: [ '' ]
		ifNotNil: [ :manifest | manifest compile: 'description ^ ' , aDescription classified: 'meta-data' ]