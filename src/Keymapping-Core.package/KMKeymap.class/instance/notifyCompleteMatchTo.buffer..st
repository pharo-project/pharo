matching
notifyCompleteMatchTo: aListener buffer: aBuffer
	| listeners |
	aListener isCollection ifTrue: [ listeners := aListener ] ifFalse: [ listeners := { aListener } ].
	listeners do: [ :l | l completeMatch: self buffer: aBuffer ].