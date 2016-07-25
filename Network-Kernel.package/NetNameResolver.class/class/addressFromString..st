address string utils
addressFromString: addressString 
	"Return the internet address represented by the given string. The string should contain four positive decimal integers delimited by periods, commas, or spaces, where each integer represents one address byte. Return nil if the string is not a host address in an acceptable format."
	"NetNameResolver addressFromString: '1.2.3.4'"
	"NetNameResolver addressFromString: '1,2,3,4'"
	"NetNameResolver addressFromString: '1 2 3 4'"
	| newAddr s byte delimiter |
	newAddr := ByteArray new: 4.
	s := addressString readStream.
	s skipSeparators.
	1 
		to: 4
		do: 
			[ :i | 
			byte := self readDecimalByteFrom: s.
			byte ifNil: [ ^ nil ].
			newAddr 
				at: i
				put: byte.
			i < 4 ifTrue: 
				[ delimiter := s next.
				(delimiter = $. or: [ delimiter = $, or: [ delimiter = $  ] ]) ifFalse: [ ^ nil ] ] ].
	^ newAddr