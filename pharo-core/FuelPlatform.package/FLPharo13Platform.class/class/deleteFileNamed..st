private-extensions-flserializationtest
deleteFileNamed: aFilename
	<flExtensionOf: #FLSerializationTest>
	 (Smalltalk at: #FileDirectory) default deleteFileNamed: aFilename