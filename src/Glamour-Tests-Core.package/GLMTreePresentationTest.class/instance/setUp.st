tests
setUp
	pane := GLMPane named: 'test'.
	presentation := GLMTreePresentation new.
	pane addPresentation: presentation.
	presentation entity: #(($a $b $c $d) ($e $f) ($g)).
	presentation children: [:x :i | x asString size > 1 ifTrue: [x] ifFalse: [OrderedCollection new]].
	presentation shouldValidate: true.