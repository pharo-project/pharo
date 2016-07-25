ui requests
onFontsChanged

	StringMorph allSubInstancesDo: [ :s | s layoutChanged ].
	TextMorph allSubInstancesDo: [ :s | s layoutChanged ].
	SystemWindow allInstancesDo: [ :w | [ w update: #relabel ] on: Error do: [ :ex | ] ].
	World ifNotNil: [ :w | w changed ].