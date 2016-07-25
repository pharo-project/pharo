composing
offset
	^ scrollPane
		ifNil: [ 0@0 ]
		ifNotNil: [ :h | scrollPane offset ]