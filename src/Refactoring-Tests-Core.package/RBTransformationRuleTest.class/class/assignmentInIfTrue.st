transformations
assignmentInIfTrue
	^self rewrite: #(
			#('``@Boolean ifTrue: [`variable := ``@true] ifFalse: [`variable := ``@false]'
			"->"
			'`variable := ``@Boolean ifTrue: [``@true] ifFalse: [``@false]')
			#('``@Boolean ifFalse: [`variable := ``@true] ifTrue: [`variable := ``@false]'
			"->"
			'`variable := ``@Boolean ifFalse: [``@true] ifTrue: [``@false]'))
		methods: false
		name: 'Move variable assignment outside of single statement ifTrue:ifFalse: blocks'