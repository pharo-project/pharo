accessing
manifestOf: elem

	^ manifestClass := elem package
		ifNil: [ nil ]
		ifNotNil: #packageManifestOrNil 
