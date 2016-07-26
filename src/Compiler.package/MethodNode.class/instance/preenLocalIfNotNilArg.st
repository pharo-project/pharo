converting
preenLocalIfNotNilArg
	"Try and spot a (var := expr) ifNil: [...] ifNotNil: [...] where var is only used in the ifNotNil: block
	 and convert it to expr ifNil: [...] ifNotNil: [:var| ...].  Deal both with the pretty-print case where
	 the block already declares the variable and the decompile case where it does not."

	| varsToHide |
	varsToHide := Set new.
	self nodesDo:
		[:node| | variable |
		(node isMessageNode
		and: [node macroPrinter == #printIfNilNotNil:indent:
		and: [node receiver isMessageNode
		and: [node receiver selector key == #==
		and: [node receiver receiver isAssignmentNode
		and: [(variable := node receiver receiver variable) isTemp
		and: [variable isRemote not
		and: [variable isOnlySubnodeOf: node in: self]]]]]]]) ifTrue:
			[node arguments last arguments isEmpty
				ifTrue: [node arguments last arguments: { variable }.
						varsToHide add: variable]
				ifFalse: [self assert: node arguments last arguments asArray =  { variable }].
			 node receiver receiver: node receiver receiver value]].
	varsToHide notEmpty ifTrue:
		[self nodesDo:
			[:node|
			((node == self or: [node isBlockNode])
			and: [node temporaries anySatisfy: [:temp| varsToHide includes: temp]]) ifTrue:
				[node temporaries: (node temporaries reject: [:temp| varsToHide includes: temp])]]]