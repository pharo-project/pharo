traits
traitsProvidingSelector: aSymbol
	| result |
	result := OrderedCollection new.
	self hasTraitComposition ifFalse: [^result].
	(self traitComposition methodDescriptionsForSelector: aSymbol)
		do: [:methodDescription | methodDescription selector = aSymbol ifTrue: [
			result addAll: (methodDescription locatedMethods
				collect: [:each | each methodClass])]].
	^result