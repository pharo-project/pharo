accessing - announcement info
sessionCreationTime
	^ self infoAt: #sessionCreationTime ifAbsent: [ SmalltalkImage current session creationTime ]