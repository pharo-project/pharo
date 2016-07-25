coverage
localCoverage
	"returns the coverage determined by a simple static analysis of test coverage 
	made by the receiver on a class that is identified by the name of the receiver.
	We assume that SetTest test Set. The computation of the coverage takes only into
	account the methods defined locally in the tested class. See coverage for a more global 
	coverage"

	| cls className |
	(self name endsWith: 'Test')
		ifFalse: [ self error: 'Please, use #localCoverageForClass: instead' ].
	className := self name copyFrom: 1 to: self name size - 'Test' size.
	cls := Smalltalk globals at: className asSymbol ifAbsent: [ self error: 'Please, use #localCoverageForClass: instead' ].
	cls isBehavior
		ifFalse: [ cls := cls class ].
	^ self localCoverageForClass: cls