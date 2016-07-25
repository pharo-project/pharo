selectors-collections
processCollectionMessagesFor: variableName in: aParseTree 
	| parent block |
	aParseTree isMessage
		ifFalse: [ ^ self ].
	(#(anyOne at: at:ifAbsent: at:ifAbsentPut: atPin: atRandom atRandom: atWrap: eight fifth first fourth last middle ninth second seventh sixth third) includes: aParseTree selector) ifTrue: [
		parent := aParseTree parent.
		(parent notNil and: [ parent isMessage ])
			ifFalse: [ ^ self ].
		aParseTree == parent receiver
			ifFalse: [ ^ self ].
		(variableMessages 
			at: (self collectionNameFor: variableName)
			ifAbsentPut: [Set new])
				add: parent selector.
		self processCollectionMessagesFor: (self collectionNameFor: variableName) in: parent ].
	(#(allSatisfy: anySatisfy: collect: collect:as: detect: detect:ifNone: detectMax: detectMin: detectSum: do: do:displayingProgress: do:separatedBy: #flatCollect: noneSatisfy: reject: select:) includes: aParseTree selector) ifTrue: [
		block := aParseTree arguments first.
		block isBlock ifFalse: [ ^ self ].
		self 
			processCollectionFor: variableName
			messagesTo: block arguments first name
			in: block ].
	(#(reduce: reduceLeft: reduceRight:) includes: aParseTree selector) ifTrue: [ 
		block := aParseTree arguments last.
		block isBlock ifFalse: [ ^ self ].
		block arguments do: [ :node |
			self
				processCollectionFor: variableName
				messagesTo: node name
				in: block ] ].
	#inject:into: = aParseTree selector ifTrue: [
		block := aParseTree arguments last.
		block isBlock ifFalse: [ ^ self ].
		self 
			processCollectionFor: variableName
			messagesTo: block arguments last name
			in: block ]