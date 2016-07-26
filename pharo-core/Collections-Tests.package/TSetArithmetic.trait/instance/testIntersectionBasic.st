tests - set arithmetic
testIntersectionBasic
	"self debug: #testIntersectionBasic"
	| inter |
	inter := self collection intersection: (self collectionClass with: self anotherElementOrAssociationIn).
	self deny: inter isEmpty.
	self assert: (inter includes: self anotherElementOrAssociationIn value)