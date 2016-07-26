private-extensions-flserializationtest
deleteFileNamed: aFilename
	<flExtensionOf: #FLSerializationTest>
	 (Smalltalk globals at: #FileDirectory) default deleteFileNamed: aFilename