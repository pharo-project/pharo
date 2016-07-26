descriptions
shortDirectoryDescription
	^ ((directory fileSystem resolve: directory path) segments last: 2) in: [ :s | s first , '/' , s last ]