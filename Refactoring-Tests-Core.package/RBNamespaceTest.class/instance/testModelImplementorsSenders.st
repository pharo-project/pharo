class tests
testModelImplementorsSenders
	| model class modelImps refs found |
	model := RBNamespace new.
	model 
		defineClass: 'Object subclass: #Asdf
			instanceVariableNames: ''''
			classVariableNames: ''''
			poolDictionaries: ''''
			category: #''Refactory-Testing'''.
	class := model classNamed: #Asdf.
	class compile: 'storeOn: aStream ^super storeOn: aStream'
		classified: #(#printing).
	modelImps := model allImplementorsOf: #storeOn:.
	self assert: (modelImps includes: class).
	self assert: modelImps size - 1 
				= (RBBrowserEnvironment new implementorsOf: #storeOn:) numberSelectors.
	refs := RBBrowserEnvironment new referencesTo: #storeOn:.
	found := false.
	model allReferencesTo: #storeOn:
		do: 
			[:each | 
			each modelClass = class 
				ifTrue: [found := true]
				ifFalse: 
					[	self flag:#todo. "temporary fix until RBEnvironments are fixed see issue 15287"
						each modelClass isMeta ifFalse:[self 
						assert: (refs includesSelector: each selector in: each modelClass realClass)]]].
	self assert: found
