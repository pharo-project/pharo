converting
ipAddressToString: byteArray
	^ String streamContents: [ :stream |
			byteArray 
				do: [ :each | stream print: each ] 
				separatedBy: [ stream nextPut: $. ] ]