Extension { #name : #CompiledMethod }

{ #category : #'*NewTools-Inspector-Extensions' }
CompiledMethod >> inpectionIr [
	<inspectorPresentationOrder: 35 title: 'IR'>

	^ SpTextPresenter new 
		text: (self ir longPrintString trimmed);
		yourself

]
