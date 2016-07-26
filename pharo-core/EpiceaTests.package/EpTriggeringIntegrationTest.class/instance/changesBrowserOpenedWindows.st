accessing
changesBrowserOpenedWindows

	^ ChangesBrowser allSubInstances reject: [ :each | each window isClosed ]