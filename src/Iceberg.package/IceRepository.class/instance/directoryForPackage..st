private
directoryForPackage: mcPackage
	| package |
	package := self packageNamed: mcPackage name.
	^ self subdirectory isEmptyOrNil
		ifTrue: [ package directoryName ]
		ifFalse: [ self subdirectory, '/', package directoryName ]