code generation (closures)
addHoistedTemps: additionalTemporaries "<SequenceableCollection>"
	additionalTemporaries do:
		[:temp|
		temp definingScope ifNil:
			[temp definingScope: self]].
	temporaries := (temporaries isNil or: [temporaries isEmpty])
					ifTrue: [additionalTemporaries copy]
					ifFalse:
						[temporaries last isIndirectTempVector
							ifTrue: [temporaries allButLast, additionalTemporaries, { temporaries last }]
							ifFalse: [temporaries, additionalTemporaries]]