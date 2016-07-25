report
leavesPrintOn: aStream tallyExact: isExact orThreshold: threshold time: totalTime
	"@TODO: Apparently, providing true as isExact will always produce an error "
	| dict |
	dict := IdentityDictionary new: 100.
	self leavesInto: dict fromSender: nil.
	isExact 
		ifTrue: [
			(self asSortedCollection: dict) do: [ :node |
				node printOn: aStream total: tally totalTime: nil tallyExact: isExact.
				node printSenderCountsOn: aStream ] ] 
		ifFalse:[
			((self asSortedCollection: dict)
				select: [ :node | node tally > threshold ]) do: [ :node |
					node printOn: aStream total: tally totalTime: totalTime tallyExact: isExact ] ]