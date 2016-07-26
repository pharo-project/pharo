testing
interval: selector 
	selector = #withLiteral ifTrue:[^ 18 to: 17].
	selector = #withSelf ifTrue:[^ 13 to: 13].
	selector = #withSuper ifTrue:[^ 13 to: 13].
	selector = #withClass ifTrue:[^ 15 to: 16].
	selector = #withClassVariable ifTrue:[^ (27 to: 26)].
	selector = #withMethod ifTrue:[^ 6 to: 5].
	selector = #withMesage ifTrue:[^ 21 to: 23].
	selector = #withClassVariable ifTrue:[^ 15 to: 16].
	selector = #withTemporaryVar ifTrue:[^ 63 to: 62].
	selector = #withInstanceVar ifTrue:[^ 18 to: 30].
	selector = #withSourceContext ifTrue:[^ 39 to: 68].
	selector = #withAssigment ifTrue:[^ 26 to: 35].
	^selectedInterval 
	
	