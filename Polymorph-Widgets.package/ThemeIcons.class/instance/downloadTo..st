private
downloadTo: dir
	| zipArchive |
	
	zipArchive := self class destinationPath / (self name, '.zip').
	zipArchive exists 
		ifFalse: [ 
			ZnClient new
				url: self url;
				downloadTo: zipArchive ].

	^ zipArchive