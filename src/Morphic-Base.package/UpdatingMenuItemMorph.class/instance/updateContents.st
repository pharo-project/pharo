private
updateContents

	target ifNotNil: [ contents := (target perform: nameSelector) ]