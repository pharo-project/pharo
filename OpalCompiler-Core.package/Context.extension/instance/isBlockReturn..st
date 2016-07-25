*OpalCompiler-Core
isBlockReturn: aPC
	"workaround, need to farward to encode"
	| bc | 
	bc := self method encoderClass  == OpalEncoderForSistaV1
		ifTrue: [ 94 ]
		ifFalse: [ 125 ].

	^ (self method at: aPC) = bc.