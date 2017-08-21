tests
testFullName

	| path |

	path := (FileSystem workingDirectory / 'book-result' / 'W01-Welcome')
				relativeToReference: FileSystem workingDirectory.
	self assert: path fullName equals: 'book-result/W01-Welcome'
