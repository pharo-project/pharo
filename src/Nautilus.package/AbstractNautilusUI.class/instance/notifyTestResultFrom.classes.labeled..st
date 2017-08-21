private
notifyTestResultFrom: packages classes: classes labeled: label
	| color |
	color := Color gray.
	(classes anySatisfy: [ :e | e hasPassedTest ])
		ifTrue: [ color := self theme successBackgroundColor ].
	(classes anySatisfy: [ :e | e hasFailedTest ])
		ifTrue: [ color := self theme warningBackgroundColor ].
	(classes anySatisfy: [ :e | e hasErrorTest ])
		ifTrue: [ color := self theme dangerBackgroundColor ].
	^ self
		notifyTitle: 'Test Finished'
		contents: label , ((packages collect: #name) joinUsing: ', ')
		color: color