Extension { #name : 'RGMethod' }

{ #category : '*Calypso-Ring' }
RGMethod >> astForStylingInCalypso [
	"this method simplified remote method styling.
	Remote proxy will redefine it by building AST on client side"
	^self ast
]

{ #category : '*Calypso-Ring' }
RGMethod >> calypsoEnvironmentType [
	^ClyMethod
]

{ #category : '*Calypso-Ring' }
RGMethod >> hasPragmaNamed: aSymbol [
	^ self pragmas anySatisfy: [ :pragma | pragma selector = aSymbol ]
]

{ #category : '*Calypso-Ring' }
RGMethod >> hasSelector: aSelector [
	"Answers true if the method refers to the selector"
	(self selector = aSelector) ifTrue: [ ^ true ].
	^ self sendsSelector: aSelector
]

{ #category : '*Calypso-Ring' }
RGMethod >> hasSelector: selector specialSelectorIndex: specialOrNil [

	^ self ast sentMessages includes: selector
]

{ #category : '*Calypso-Ring' }
RGMethod >> implementors [

	^ #()
]

{ #category : '*Calypso-Ring' }
RGMethod >> isInstalled [
	^ true
]

{ #category : '*Calypso-Ring' }
RGMethod >> literalAt: anIndex [

	^ self ast literals at: anIndex
]

{ #category : '*Calypso-Ring' }
RGMethod >> messages [

	^ self ast sentMessages
]

{ #category : '*Calypso-Ring' }
RGMethod >> origin [

	^ self parent
]

{ #category : '*Calypso-Ring' }
RGMethod >> pragmas [

	^ self ast pragmas
]

{ #category : '*Calypso-Ring' }
RGMethod >> readsRef: literalAssociation [
	"Answer whether the receiver loads the argument."
	^ self ast allChildren anySatisfy: [:each |
		each isVariable and: [ each name = literalAssociation key ]]

	"self ast nodesDo: [ :node |
		node isVariable and: [
			node parent isAssignment not and: [
				(node binding =  literalAssociation)
					 		ifTrue: [^true]]]].
	^false"
]

{ #category : '*Calypso-Ring' }
RGMethod >> readsSlot: aSlot [
	| nodes |
	nodes := self ast instanceVariableNodes select: [:each | each isInstanceVariable].
	nodes := nodes reject: [ :node | node parent isAssignment and: [ node parent variable = node ] ].
	^ nodes anySatisfy: [ :node | node binding == aSlot ]
]

{ #category : '*Calypso-Ring' }
RGMethod >> realMethod [
	"Included to support for browsing unloaded methods, i.e. from the Calypso. 
	Since Calypso still uses Ring objects, it needs to get the real method so it can understand messages such as #isClassified. The real fix here is to reify Package support (like tags) in Ring (which is planned to be removed). See issue #15931 for details"

	| resolvedName |
	
	resolvedName := self parent isMeta
		ifTrue: [ (self class environment at: self parent instanceSide resolvedNameOrNil) class ]
		ifFalse: [ self class environment at: self parent resolvedNameOrNil ].

	^ resolvedName
		ifNotNil: [ : realClass | ^ realClass >> self name ]
		ifNil: [ self error: 'Unexpected error while resolving Ring method class' ]
]

{ #category : '*Calypso-Ring' }
RGMethod >> sendNodes [
	^self ast sendNodes
]

{ #category : '*Calypso-Ring' }
RGMethod >> sendsSelector: aSymbol [
	"Answer whether the method sends a particular selector"
	^ self messages includes: aSymbol
]

{ #category : '*Calypso-Ring' }
RGMethod >> variableNodes [
	^self ast variableNodes
]

{ #category : '*Calypso-Ring' }
RGMethod >> writesRef: literalAssociation [
	"Answer whether the receiver stores into the argument."
	self ast nodesDo: [ :node |
		node isVariable and: [
			node parent isAssignment and: [
				(node parent variable = node) and: [
					(node binding =  literalAssociation)
					 		ifTrue: [^true]]]]].
	^false
]

{ #category : '*Calypso-Ring' }
RGMethod >> writesSlot: aSlot [
	self ast nodesDo: [ :node |
		node isVariable and: [
			node parent isAssignment and: [
				(node parent variable = node) and: [
					node isInstanceVariable and: [
						(node binding ==  aSlot)
					 		ifTrue: [^true]]]]]].
	^false
]
