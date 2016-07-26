UI specific
showResult: anObject
	self result: anObject.
	resultPane
		ifNil: [resultPane := win
			newTextEditorFor: self
			getText: #resultText
			setText: nil
			getEnabled: nil.
			codeTabPane addPage: resultPane label: 'Result']
		ifNotNil: [self changed: #resultText]

	