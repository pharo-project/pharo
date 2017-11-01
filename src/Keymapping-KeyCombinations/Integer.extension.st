Extension { #name : #Integer }

{ #category : #'*Keymapping-KeyCombinations' }
Integer >> alt [
	^ KMModifier alt + self
]

{ #category : #'*Keymapping-KeyCombinations' }
Integer >> asKeyCombination [
	^ KMSingleKeyCombination from: self asCharacter
]

{ #category : #'*Keymapping-KeyCombinations' }
Integer >> asShortcut [
	^ self asKeyCombination
]

{ #category : #'*Keymapping-KeyCombinations' }
Integer >> command [
	^ KMModifier command + self
]

{ #category : #'*Keymapping-KeyCombinations' }
Integer >> control [
	^ KMModifier ctrl + self
]

{ #category : #'*Keymapping-KeyCombinations' }
Integer >> ctrl [
	^ KMModifier ctrl + self
]

{ #category : #'*Keymapping-KeyCombinations' }
Integer >> meta [
	^ KMModifier meta + self
]

{ #category : #'*Keymapping-KeyCombinations' }
Integer >> shift [
	^ KMModifier shift + self
]
