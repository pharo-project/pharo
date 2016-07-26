printing/formatting
formatBytecode: aBytecode
	| textAttributes|
	
	textAttributes := {TextColor new color: Smalltalk ui theme textColor}.
	self selectedContext pc = aBytecode offset ifTrue: [ textAttributes := {TextColor new color: Smalltalk ui theme textColor. TextEmphasis italic} ]. 
	self selectedContext pc > aBytecode offset ifTrue: [ textAttributes := {TextColor new color: Color gray} ].
	^ Text 
		string: aBytecode printString
		attributes: textAttributes