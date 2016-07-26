private
saveAsImageAsTestImage
	"Now you can run tests and see and go back to the previous image
	and do that go and back.... then after you can decide to freeze"

	| file |
	file := FileSystem workingDirectory  /  (self shortImageName, '-imageForTests') , Smalltalk imageSuffix.
	Smalltalk image saveAs: file nextVersion basename.