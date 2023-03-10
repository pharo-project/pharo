Extension { #name : #TraitedClass }

{ #category : #'*CodeExport-Traits' }
TraitedClass >> fileOutLocalMethodsInCategory: aProtocol on: aFileStream [

	| selectors |
	aFileStream cr.
	selectors := self selectorsToFileOutCategory: aProtocol.

	selectors do: [ :selector |
		((self isLocalSelector: selector) or: [ (self traitComposition selectors includes: selector) not ]) ifTrue: [ self printMethodChunk: selector on: aFileStream ] ].

	^ self
]
