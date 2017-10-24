tests
testColorFrom
	self assert: ((Color colorFrom: #white) asHexString sameAs: 'ffffff').
	self assert: ((Color colorFrom: #(1.0 0.5 0.0)) asHexString sameAs: 'ff8000').
	self assert: ((Color colorFrom: (Color white)) asHexString sameAs: 'ffffff').
	self assert: ((Color colorFrom: '#FF8800') asHexString sameAs: 'ff8800').
	self assert: ((Color colorFrom: '#222222') asHexString sameAs: '222222').