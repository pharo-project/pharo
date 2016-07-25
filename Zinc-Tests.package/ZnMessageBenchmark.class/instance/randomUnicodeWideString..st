private
randomUnicodeWideString: count
	| alphabet |
	"This is about 6% non-ASCII characters, 6% non-octet characters, always a WideString"
	alphabet := '0123456789ABCDEF-élève en Français-0123456789ABCDEF', 
		(WideString withAll: (#(300 400 500) collect: [ :each | each asCharacter ])).
	^ WideString
		new: count
		streamContents: [ :stream | 
			count timesRepeat: [ stream nextPut: alphabet atRandom ] ]