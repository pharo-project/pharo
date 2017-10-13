instruction stream support
isSendAt: pc in: method
	"Answer whether the bytecode at pc is a message-send."

	"	96-111		0110 iiii			Send Arithmetic Message #iiii #(#+ #- #< #> #'<=' #'>=' #= #'~=' #* #/ #'\\' #@ #bitShift: #'//' #bitAnd: #bitOr:)
		112-119	01110 iii			Send Special Message #iii #(#at: #at:put: #size #next #nextPut: #atEnd #'==' class)
		120		01111000			UNASSIGNED (was: blockCopy:)
		121		01111001			Send Special Message #value
		122-123	0111101 i			Send Special Message #i #(#value: #do:)
		124-127	011111 ii			Send Special Message #ii #(#new #new: #x #y))
		128-143	1000 iiii			Send Literal Selector #iiii With 0 Argument
		144-159	1001 iiii			Send Literal Selector #iiii With 1 Arguments
		160-175	1010 iiii			Send Literal Selector #iiii With 2 Arguments
	 **	234		11101010	iiiiijjj	Send Literal Selector #iiiii (+ Extend A * 32) with jjj (+ Extend B * 8) Arguments
	 **	235		11101011	iiiiijjj	Send To Superclass Literal Selector #iiiii (+ Extend A * 32) with jjj (+ Extend B * 8) Arguments"

	| byte |
	byte := self nonExtensionBytecodeAt: pc in: method.
	^byte >= 96
	  and: [byte <= 175
		 or: [byte >= 234 and: [byte <= 235]]]