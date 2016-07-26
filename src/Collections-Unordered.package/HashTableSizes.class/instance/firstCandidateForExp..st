private
firstCandidateForExp: exp
	"Answer the smallest odd integer greater 2**exp."
	
	| n |
	n := (2 raisedTo: exp) rounded.
	^n odd 
		ifTrue: [n]
		ifFalse: [n + 1]