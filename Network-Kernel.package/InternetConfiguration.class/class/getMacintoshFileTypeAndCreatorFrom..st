lookups
getMacintoshFileTypeAndCreatorFrom: aFileName
	"Return the application type and application signature for the file
	 for the macintosh file system based on the file ending, the file does not need to exist
	failure to find a signature based on the file ending, or because of primitive failure turns nil"
	"InternetConfiguration getMacintoshFileTypeAndCreatorFrom: 'test.jpg'"
	| string |

	string := self primitiveGetMacintoshFileTypeAndCreatorFrom: aFileName.
	string = '********' ifTrue: [^nil].
	^Array with: (string first: 4) with: (string last: 4)
