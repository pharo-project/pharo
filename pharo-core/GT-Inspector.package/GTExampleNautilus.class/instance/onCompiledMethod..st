public
onCompiledMethod: aBuilder
	| target method examples |
	target := aBuilder model.
	(method := target selectedMethod) ifNil: [ ^ self ].
	self onReferences: method selector builder: aBuilder.
	(examples := method gtExamplesContained) isEmptyOrNil ifTrue: [ ^ self ].
	self onExamples: examples builder: aBuilder
