tests
testHex
	"self debug: #testHex"
	
	self assert: #[122 43 213 7] hex = '7a2bd507'.
	self assert: #[151 193 242 221 249 32 153 72 179 41 49 154 48 193 99 134] hex = '97c1f2ddf9209948b329319a30c16386'.
					
	self assert: (ByteArray readHexFrom: '7A2BD507') = #[122 43 213 7].
	self assert: (ByteArray readHexFrom: '7a2bd507') = #[122 43 213 7].
	