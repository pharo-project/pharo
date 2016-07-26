private
priorVersionOfAMethod: aChangeRecord
	| index mylist |
	
	mylist := referencesList sorted: [:a :b | a timeStamp > b timeStamp ].
	index := mylist indexOf: aChangeRecord .
	^ mylist at: index+1 ifAbsent: [ aChangeRecord ]