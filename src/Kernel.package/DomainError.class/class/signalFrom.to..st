signaling
signalFrom: start to: end
	| msgStart msgEnd |
	msgStart := (start isFloat and: [start isFinite not]) ifTrue: ['(-infinity'] ifFalse: ['[', start printString].
	msgEnd := (end isFloat and: [end isFinite not]) ifTrue: ['infinity)'] ifFalse: [end printString, ']'].
	^ self signal: 'Value outside ', msgStart, ' , ' , msgEnd
		from: start;
		to: end