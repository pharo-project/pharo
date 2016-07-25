accessing
reference: dir
	| tmpReference |
	"Set the path of the volume to be displayed."
	self okToChange ifFalse: [^ self].
	
	tmpReference := dir 
		ifNotNil: [ :d| d asFileReference]
		ifNil: [ FileSystem disk workingDirectory ].
	tmpReference isReadable 
		ifFalse: [ ^ self inform: ('Cannot read {1}' translated format: { tmpReference})].
	reference := tmpReference.
	sortMode ifNil: [sortMode := #date].
	
	volumeList := ((Array with: '[]'), reference pathSegments)  
				withIndexCollect: [:each :i | ( String new: i-1 withAll: $ ), each].
	
	volumeListIndex  := volumeList size.	
		
	self changed: #relabel.
	self changed: #volumeList.
	self updateFileList.