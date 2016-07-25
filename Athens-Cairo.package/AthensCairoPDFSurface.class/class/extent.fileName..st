instance creation
extent: anExtent fileName: fileName

	^ (self createPDF: fileName width: anExtent x height: anExtent y) initialize