tests
testSymbols
	self class compile: 'array ^ #(#nil #true #false #''nil'' #''true'' #''false'')'.
	self assert: self array = {#nil. #true. #false. #nil. #true. #false}.