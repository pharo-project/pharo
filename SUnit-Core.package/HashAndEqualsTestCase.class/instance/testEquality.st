testing
testEquality
	"Check that TextFontChanges report equality correctly"
	prototypes
		do: [:p | self
				should: [(EqualityTester with: p) result]] 