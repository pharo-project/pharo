accessing
textView
	^ self model ifNotNil: [ :m | m textArea ]