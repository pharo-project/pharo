tests
testConstructionUsingWith
	"Use the with: constructor."

	| aStream |
	aStream := ReadWriteStream with: #(1 2).
	self assert: (aStream contents = #(1 2)) description: 'Ensure correct initialization.'