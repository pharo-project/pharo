Extension { #name : 'SystemNavigation' }

{ #category : '*HeuristicCompletion-Model' }
SystemNavigation >> allSentMessagesInClass: aClass [
	"Answer the set of selectors which are sent somewhere in the system."
	| sent |
	sent := IdentitySet new.
	self allSentMessagesInClass: aClass do: [ :e | sent add: e ].
	^ sent
]

{ #category : '*HeuristicCompletion-Model' }
SystemNavigation >> allSentMessagesInClass: aClass do: aBlock [
	"Answer the set of selectors which are sent somewhere in the system."
	aClass methodsDo: [:method | "Include all sels, but not if sent by self"
		method literalsDo: [:literal |
			(literal isSymbol) ifTrue: ["might be sent"
				literal == method selector ifFalse: [ aBlock value: literal ]].
			(literal isMemberOf: Array) ifTrue: ["might be performed"
				literal do: [:x | (x isSymbol) ifTrue: [ x == method selector ifFalse: [ aBlock value: x ]]]]]].
	"The following may be sent without being in any literal frame"
	1
		to: Smalltalk specialSelectorSize
		do: [:index | aBlock value: (Smalltalk specialSelectorAt: index)]
]

{ #category : '*HeuristicCompletion-Model' }
SystemNavigation >> allSentMessagesInPackage: aPackage [
	| sent |
	sent := IdentitySet new.
	self allSentMessagesInPackage: aPackage do: [ :e | sent add: e ].
	^ sent
]

{ #category : '*HeuristicCompletion-Model' }
SystemNavigation >> allSentMessagesInPackage: aPackage do: aBlock [
	"Answer the set of selectors which are sent somewhere in the system."

	aPackage definedClasses do: [ :class | self allSentMessagesInClass: class do: aBlock ]
]
