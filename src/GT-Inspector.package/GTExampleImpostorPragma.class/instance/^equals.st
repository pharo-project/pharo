comparing
= aImpostorPragma
	^ self class == aImpostorPragma class
		and: [ self method == aImpostorPragma method ]