tests
testDate29Feb2000
	"Ensure that dates with leap years don't screw up in the conversion"

	| archive member theDate |
	theDate := Date year: 2000 month: 2 day: 29.
	archive := ZipArchive new.
	member := archive addDeflateString: 'foo' as: 'bar'.
	member modifiedAt: theDate.
	self assert: member lastModTime equals: theDate.