initialize-release
createFileSystem
	^ FileSystem store: (DiskStore activeClass createDefault)