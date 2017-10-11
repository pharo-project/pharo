class initialization
nonCharacters
	nonCharacters
		ifNil: [ nonCharacters := (16rFDD0 to: 16rFDEF) asOrderedCollection
				, #(16rFFFE 16rFFFF 16r1FFFE 16r1FFFF 16r0002FFFE 16r0002FFFF 16r0003FFFE 16r0003FFFF)
				, (16r4FFFE to: 16rFFFFE by: 16r10000) , (16r4FFFF to: 16rFFFFF by: 16r10000)
				, #(16r0010FFFE 16r0010FFFF) ].
	^ nonCharacters