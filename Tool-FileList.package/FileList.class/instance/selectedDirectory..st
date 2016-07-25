private
selectedDirectory: aFileDirectoryWrapper
	| file |
	file := aFileDirectoryWrapper withoutListWrapper.
	file ifNil: [ 
		brevityState := #FileList.
		self changed: #fileList.
		self changed: #selectedDirectory.
		^ self ].
	self directory: file.
	brevityState := #FileList.
	"self addPath: path."
	self changed: #fileList.
	self changed: #contents.
	self changed: #selectedDirectory.