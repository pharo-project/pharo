utilties
top128FromUnicodeSpec: mapping
	"Return an array mapping the top 128 byte to unicode values from a Unicode.org specification map"

	^ Array new: 128 streamContents: [ :stream |
		128 to: 255 do: [ :each |
			stream nextPut: (mapping at: each ifAbsent: [ nil ]) ] ]