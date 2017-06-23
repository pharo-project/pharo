utility
uuidFromCommit: aCommit package: aPackage.
	^ self uuidFromSHA: (SHA1 new hashStream: (ReadStream on: aCommit id, aPackage name))
