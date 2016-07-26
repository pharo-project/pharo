accessing
triggererReferenceOf: anEntry ifPresent: presentBlock ifAbsent: absentBlock

	^ anEntry tags 
		at: self class triggererReferenceKey 
		ifPresent: presentBlock 
		ifAbsent: absentBlock