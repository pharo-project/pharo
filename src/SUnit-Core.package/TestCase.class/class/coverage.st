coverage
coverage
	"returns the coverage determined by a simple static analysis of test coverage 
	made by the receiver on a class that is identified by the name of the receiver.
	We assume that SetTest test Set."

	| cls className |
	(self name endsWith: 'Test')
		ifFalse: [ self error: 'Please, use #coverageForClass: instead' ].
	className := self name copyFrom: 1 to: self name size - 'Test' size.
	cls := Smalltalk globals at: className asSymbol ifAbsent: [ self error: 'Please, use #coverageForClass: instead' ].	"May happen with Transcript"
	cls isBehavior
		ifFalse: [ cls := cls class ].
	^ self coverageForClass: cls