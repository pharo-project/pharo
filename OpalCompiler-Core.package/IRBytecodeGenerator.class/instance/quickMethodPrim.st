private
quickMethodPrim

	| index |
	self numArgs = 0 ifFalse: [^ 0].
	lastSpecialReturn ifNil: [^ 0].
	(seqBytes  size <= 2) ifFalse: [^ 0].
	"this is for ruling out the case in which the structure is the same as a quick return
	but with and invalid special literal."
	((literals size = 1) and: [ (encoderClass quickPrimSpecialConstants identityIncludes: literals first) not and: [ lastSpecialReturn selector = #returnConstant: ] ] )
		ifTrue: [^ 0].
	lastSpecialReturn selector == #returnReceiver 
		ifTrue: [^256].
	lastSpecialReturn selector == #returnConstant:
		 ifTrue: [^(index := encoderClass quickPrimSpecialConstants identityIndexOf: lastSpecialReturn argument) > 0
					ifTrue: [256 + index] ifFalse: [0]].
	lastSpecialReturn selector == #returnInstVar: 
		ifTrue: [^forceLongForm 
					ifTrue: [0]  "when compiling long bytecodes for Contexts, do not do quick return either"
					ifFalse: [263 + lastSpecialReturn argument]]
	