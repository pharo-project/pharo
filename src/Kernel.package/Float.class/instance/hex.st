printing
hex  

	^ String streamContents:
		[:strm | | word nibble |
		1 to: 2 do:
			[:i | word := self at: i.
			1 to: 8 do: 
				[:s | nibble := (word bitShift: -8+s*4) bitAnd: 16rF.
				strm nextPut: ('0123456789ABCDEF' at: nibble+1)]]]
"
(-2.0 to: 2.0) collect: [:f | f hex]
"