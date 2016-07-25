execute
execute
	actionBlock
		cull: table dataSource
		cull: field getTextFromModel asString
		cull:
			(table selectedRowIndex = 0
				ifFalse: [ table dataSource realElementAt: table selectedRowIndex ]
				ifTrue: [ nil ])