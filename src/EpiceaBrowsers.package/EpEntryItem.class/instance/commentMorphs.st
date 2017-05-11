private
commentMorphs
	
	browser commentsLog
		commentAt: entry 
		ifPresent: [ :comment |
			comment trim ifNotEmpty: [:trimmedComment |
				^ { self morphFactory pillWith: trimmedComment } ] ].
	^ #()