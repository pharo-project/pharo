open/close
openSelectedFile
	"Open a stream on the selected file if available and return it."

	|d f fileRef |
	d := self selectedFileDirectory ifNil: [^nil].
	f := self selectedFileName ifNil: [^nil].
	self selectedFileEntry isDirectory ifTrue: [^nil].
	fileRef := d/f.
	^ fileRef exists ifTrue: [ fileRef readStream ] ifFalse: nil  