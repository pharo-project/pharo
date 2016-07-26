accessing-dynamic
currentText
	^ self current ifNil: [''] ifNotNil: [ self current text ]