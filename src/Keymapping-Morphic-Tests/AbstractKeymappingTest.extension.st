Extension { #name : 'AbstractKeymappingTest' }

{ #category : '*Keymapping-Morphic-Tests' }
AbstractKeymappingTest >> eventKey: character [
	^ self eventKey: character
		alt: false
		ctrl: false
		command: false
		shift: false
]

{ #category : '*Keymapping-Morphic-Tests' }
AbstractKeymappingTest >> eventKey: character alt: aBoolean [
	^ self eventKey: character
		alt: aBoolean
		ctrl: false
		command: false
		shift: false
]

{ #category : '*Keymapping-Morphic-Tests' }
AbstractKeymappingTest >> eventKey: character alt: useAlt ctrl: useCtrl command: useCommand shift: useShift [
	| event modifier code |
	event := KeyboardEvent new.
	modifier := 0.

	useShift
		ifTrue: [ modifier := modifier + 8].
	useCtrl
		ifTrue: [ modifier := modifier + 16].
	useAlt
		ifTrue: [ modifier := modifier + 32].
	useCommand
		ifTrue: [ modifier := modifier + 64].

	code := character asInteger.
	event
		setType: #keystroke
		buttons: modifier
		position:  0@0
		keyValue: code
		charCode: code
		hand: nil
		stamp: Time now.
	event key: (KeyboardKey fromCharacter: character).
	^ event
]

{ #category : '*Keymapping-Morphic-Tests' }
AbstractKeymappingTest >> eventKey: character command: aBoolean [
	^ self eventKey: character
		alt: false
		ctrl: false
		command: aBoolean
		shift: false
]

{ #category : '*Keymapping-Morphic-Tests' }
AbstractKeymappingTest >> eventKey: character ctrl: aBoolean [
	^ self eventKey: character
		alt: false
		ctrl: aBoolean
		command: false
		shift: false
]

{ #category : '*Keymapping-Morphic-Tests' }
AbstractKeymappingTest >> eventKey: character shift: aBoolean [
	^ self eventKey: character
		alt: false
		ctrl: false
		command: false
		shift: aBoolean
]
