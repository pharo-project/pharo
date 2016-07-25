private
priorVersionOfAMethod: aChangeRecord
	| index |
	index := referencesList identityIndexOf: aChangeRecord .
	^ referencesList at: index+1 ifAbsent: [ nil ]