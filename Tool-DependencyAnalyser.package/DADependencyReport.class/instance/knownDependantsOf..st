accessing
knownDependantsOf: aPackageName
	^ dependencies associations 
		select: [ :association | association value includes: aPackageName ]
		thenCollect: #key