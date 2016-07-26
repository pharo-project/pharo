operations
performOperation
	"Download the HTTP(S) resource specified by url to the file in argument.
	Return a file reference on the file that's just been downloaded.
	If something goes wrong, an exception will be signalled."

	ZnClient new
		enforceHttpSuccess: true;
		url: self url;
		downloadTo: argument.
	^ argument asFileReference