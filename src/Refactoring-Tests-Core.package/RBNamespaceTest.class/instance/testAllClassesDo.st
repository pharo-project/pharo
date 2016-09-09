class tests
testAllClassesDo
	| model classes |
	classes := 0.
	model := RBClassModelFactory rbNamespace new.
	model allClassesDo: 
			[:each | 
			each name = #Object ifTrue: [each allSubclasses].
			classes := classes + 1].
	RBBrowserEnvironment new classesDo: [:each | classes := classes - 1].
	self assert: classes = 0