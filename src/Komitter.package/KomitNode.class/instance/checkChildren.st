private
checkChildren
	
	self silentWhile: [
		(self children value allSatisfy: [ :e | e selected ])
			ifTrue: [ self selected: true ]
			ifFalse: [ self selected: false ] ]