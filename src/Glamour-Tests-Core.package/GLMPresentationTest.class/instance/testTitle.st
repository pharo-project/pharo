tests
testTitle
	| presentation |
	presentation := GLMPresentation new.
	presentation entity: #foo.
	self assert: presentation titleValue isNil.
	presentation title: '41'.
	self assert: presentation titleValue equals: '41'.
	presentation entity: '4'.
	presentation title: [ :x | x , '2' ].
	self assert: presentation titleValue equals: '42'.
	presentation entity: (GLMMultiValue withAll: #('4' '3')).
	presentation title: [ :first :second | first , second ].
	self assert: presentation titleValue equals: '43'