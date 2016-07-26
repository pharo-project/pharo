unzipping
unzip: fullFileName to: pathName
	"Unzip the contents of the file specified by the full path name or file reference fullFileName to the location given by pathName.
	GZipReadStream unzip:'/aPath/zip.gz' to: '/aPath'
	GZipReadStream unzip:'zip.gz' to: '/aPath'
	GZipReadStream unzip:'/aPath/zip.gz' asFileReference to: '/aPath' asFileReference
	
	To unzip a .zip go to the class ZipArchive
	"
	
	| zipped buffer unzipped newName path|
	newName := fullFileName asFileReference basename copyUpToLast: FileSystem disk extensionDelimiter.
	path := pathName asFileReference.
	path ensureCreateDirectory.
	unzipped := FileStream newFileNamed: (path / newName) fullName.
	unzipped ifNil: [self error: pathName, ' looks incorrect'].
	[ unzipped binary. 
	zipped := self on: (FileStream readOnlyFileNamed: fullFileName).
	buffer := ByteArray new: 50000.
	[zipped atEnd] whileFalse: [unzipped nextPutAll: (zipped nextInto: buffer)]]
		ensure: [
			zipped close.
			unzipped close].
	^ newName