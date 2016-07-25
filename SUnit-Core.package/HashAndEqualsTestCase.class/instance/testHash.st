testing
testHash
	"test that TextFontChanges hash correctly"
	prototypes do: [:p |
		self should: [(HashTester with: p) result]] 