private
stagedPackageNames
	^ self stagedFiles 
		select: [ :fileRef | self isFilePartOfAPackage: fileRef ]
		thenCollect: [ :fileRef | 
			self subdirectory isEmptyOrNil 
				ifTrue: [ fileRef path segments first asFileReference basenameWithoutExtension ] 
				ifFalse: [ fileRef path segments second asFileReference basenameWithoutExtension ] ]