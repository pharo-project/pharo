Extension { #name : #KeyboardKey }

{ #category : #'*Keymapping-KeyCombinations' }
KeyboardKey >> alt [

	^ KMModifier alt + self
]

{ #category : #'*Keymapping-KeyCombinations' }
KeyboardKey >> asKeyCombination [

	^ KMSingleKeyCombination new
		  key: self;
		  yourself
]

{ #category : #'*Keymapping-KeyCombinations' }
KeyboardKey >> command [

	^ KMModifier command + self
]

{ #category : #'*Keymapping-KeyCombinations' }
KeyboardKey >> control [

	^ KMModifier control + self
]

{ #category : #'*Keymapping-KeyCombinations' }
KeyboardKey >> ctrl [

	^ KMModifier ctrl + self
]

{ #category : #'*Keymapping-KeyCombinations' }
KeyboardKey >> meta [

	^ KMModifier meta + self
]

{ #category : #'*Keymapping-KeyCombinations' }
KeyboardKey >> shift [

	^ KMModifier shift + self
]
