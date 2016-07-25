visiting
visitReturn: ret
	prevInstr ifNil: [ ^ self ].
	prevInstr canBeQuickReturn ifFalse: [ ^ self ].
	retToFix add: prevInstr -> ret.