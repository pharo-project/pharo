Extension { #name : #Character }

{ #category : #'*Keymapping-KeyCombinations' }
Character >> alt [
	^ KMModifier alt + self
]

{ #category : #'*Keymapping-KeyCombinations' }
Character >> asKeyCombination [
	| lowerCaseCombination |
	self asLowercase = self ifTrue: [^ KMSingleKeyCombination from: self].
	lowerCaseCombination := KMSingleKeyCombination from: self asLowercase.
	^KMModifiedKeyCombination modifier: KMModifier shift character: lowerCaseCombination.
]

{ #category : #'*Keymapping-KeyCombinations' }
Character >> asShortcut [
	^ self asKeyCombination
]

{ #category : #'*Keymapping-KeyCombinations' }
Character >> command [
	^ KMModifier command + self
]

{ #category : #'*Keymapping-KeyCombinations' }
Character >> control [
	^ KMModifier control + self
]

{ #category : #'*Keymapping-KeyCombinations' }
Character >> ctrl [
	^ KMModifier ctrl + self
]

{ #category : #'*Keymapping-KeyCombinations' }
Character >> meta [
	^ KMModifier meta + self
]

{ #category : #'*Keymapping-KeyCombinations' }
Character >> shift [
	^ KMModifier shift + self
]
