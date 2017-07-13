accessing
manifestOf: elem

	^ manifestClass := elem package ifNotNil: #packageManifestOrNil 
