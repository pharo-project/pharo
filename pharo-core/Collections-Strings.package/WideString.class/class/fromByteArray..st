instance creation
fromByteArray: aByteArray 

	| inst |
	aByteArray size \\ 4 = 0 ifFalse: [^ ByteString fromByteArray: aByteArray ].
	inst := self new: aByteArray size // 4.
	4 to: aByteArray size by: 4 do: [:i |
		inst basicAt: i // 4
			put: ((aByteArray at: i - 3) << 24) + 
				((aByteArray at: i - 2) << 16) +
				 ((aByteArray at: i - 1) << 8) +
				(aByteArray at: i)
	].

	^ inst
