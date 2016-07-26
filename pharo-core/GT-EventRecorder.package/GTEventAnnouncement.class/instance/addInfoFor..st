updating
addInfoFor: aGTSpotterRecorderEvent 
	(aGTSpotterRecorderEvent respondsTo: #infoAt:put:) ifTrue: [
		dictionary keysAndValuesDo: [ :key :value |
			key = #data ifFalse: [ 
				aGTSpotterRecorderEvent infoAt: key put: value ] ] ]