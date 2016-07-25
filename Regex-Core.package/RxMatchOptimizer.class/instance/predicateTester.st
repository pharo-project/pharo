private
predicateTester

	| p pred |
	predicates isEmpty ifTrue: [^nil].
	p := self optimizeSet: predicates.	"also allows copying closures"
	^p size = 1
		ifTrue: [
			pred := p first.
			[:char :matcher | 
				pred value: char ]]
		ifFalse: [
			[:char :matcher | 
				p contains: [:some | some value: char ]]]