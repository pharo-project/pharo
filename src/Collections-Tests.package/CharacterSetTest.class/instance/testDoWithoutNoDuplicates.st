test - iterate
testDoWithoutNoDuplicates
	"self debug: #testDoWithoutNoDuplicates"

	| res |
	res := self speciesClass new.
	self collection do: [ :each | res add: each ] without: $a.
	self assert: res size equals: self doWithoutNumber