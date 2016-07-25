system attribute
extractMinusParametersFrom: arguments
	| dict previous previousWithMinus args |
	dict := Dictionary new.
	arguments isEmptyOrNil
		ifTrue: [ ^ dict ].
	args := OrderedCollection withAll: arguments.
	previous := previousWithMinus := args detect: [ :each | each first = $- ] ifNone: [ String space ].
	[ args isEmpty or: [ args first = previous ] ]
		whileFalse: [ 
			dict at: #* put: (dict at: #* ifAbsent: [ #() ]) , (Array with: args first).
			args removeFirst ].
	args
		do: [ :each | 
			each first = $-
				ifTrue: [ 
					previous first = $-
						ifTrue: [ dict at: previous allButFirst put: #() ].
					previousWithMinus := each ]
				ifFalse: [ 
					dict
						at: previousWithMinus allButFirst
						put: (dict at: previousWithMinus allButFirst ifAbsent: [ #() ]) , (Array with: each) ].
			previous := each ].
	previous first = $-
		ifTrue: [ dict at: previous allButFirst put: #() ].
	^ dict