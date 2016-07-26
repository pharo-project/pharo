private
process: anExample withArguments: aBlock
	| arguments |
	arguments := nil.
	anExample hasValidArguments 
		ifFalse: [ ^ self onArgumentError value: self value: anExample value: arguments ].
	[ arguments := aBlock value ]
		on: Error
		do: [ :exception | ^ self onDependencyError value: self value: anExample value: exception ].
	self action ifNotNil: [ self example == anExample ifFalse: [ self action value: anExample ] ]