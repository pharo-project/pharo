accessing
packageComment
	^ self packageManifestOrNil ifNil: [ '' ] ifNotNil: [ :manifest | manifest description ]