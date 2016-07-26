interpreting-private
extractArrayToInterpretFrom: aFragment

	arrayToInterpret := aFragment.
	
	aFragment isSpecLayout
		ifTrue: [ arrayToInterpret := aFragment asArray ].
	
	(arrayToInterpret isCollection and: [ arrayToInterpret isSequenceable ])
		ifFalse: [ ^ arrayToInterpret ].
	
	(aFragment isString or: [ aFragment isText ])
		ifTrue: [ 
			^ aFragment == #model
				ifTrue: [ model ]
				ifFalse: [ self convertSymbolOfClassToInstance: aFragment ] ].
	^ nil