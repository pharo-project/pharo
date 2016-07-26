private
validateDoIts: aSpec versionString: versionString errorMessage: errorMessage
	| ar |
	ar := {
		#preLoadDoIt.	'preLoadDoIt selector for '.
		#postLoadDoIt.	'postLoadDoIt selector for '.
	}.
	1 to: ar size by: 2 do: [ :i | 
		| selector |
		selector := (aSpec perform: (ar at: i)) value.
		selector ~~ nil
			ifTrue: [ 
				selector isSymbol
					ifFalse: [ 
						self
							recordValidationError: (ar at: i + 1) , errorMessage , ' is not a symbol'
							versionString: versionString
							callSite: #validateDoIts:versionString:errorMessage:
							reasonCode: #invalidDoItSelector ] ] ]