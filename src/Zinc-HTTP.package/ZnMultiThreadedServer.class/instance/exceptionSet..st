private
exceptionSet: classNames
	^ classNames
		inject: ExceptionSet new
		into: [ :set :each |
			(Smalltalk includesKey: each)
				ifTrue: [ set add: (Smalltalk at: each); yourself ]
				ifFalse: [ set ] ]