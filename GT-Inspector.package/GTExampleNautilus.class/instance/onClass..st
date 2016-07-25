public
onClass: aBuilder
	| target class examples |
	target := aBuilder model.
	(class := target selectedClass) ifNil: [ ^ self ].
	self onReferences: class theNonMetaClass name builder: aBuilder.
	self onProvider: class theNonMetaClass builder: aBuilder.
	self onSource: class theNonMetaClass builder: aBuilder.
	(examples := class theNonMetaClass gtExamplesContained) isEmptyOrNil ifTrue: [ ^ self ].
	self onExamples: examples builder: aBuilder