helpers
numberOfExistingTempStreamFiles
	"This answers the number of files that were created for mapping standard files.
	Note that in OSSUnixSubprocessTest >> newCommand we define that temp files must be created in /tmp
	and in OSSUnixSubprocess >> createTempFilename we can see the format of the name 
	"
	^ ('/tmp' asFileReference entries select: [ :each | 
		(each name beginsWith: 'OSSUnixSubprocess-p')  and: [ each name endsWith: '.deleteme' ]  ]) size

	