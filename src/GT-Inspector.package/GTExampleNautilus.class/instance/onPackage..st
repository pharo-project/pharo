public
onPackage: aBuilder
	| target package examples |
	target := aBuilder model.
	(package := target selectedPackage) ifNil: [ ^ self ].
	package item ifNil: [ ^ self ].
	(examples := package item gtExamplesContained) isEmptyOrNil ifTrue: [ ^ self ].
	self onExamples: examples builder: aBuilder