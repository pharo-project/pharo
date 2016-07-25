organization updating
noteChangesFrom: oldMethodDict
	"create notifications about the changes made to the method dictionary"
	
	"deal with additions"
	self methodDict keysAndValuesDo:  [ :selector :currentMethod|
		oldMethodDict 
			at: selector 
			ifAbsent: [ self noteMethodAdded: currentMethod ]].
	
	" deal with removal / updates"
	oldMethodDict keysAndValuesDo: [ :selector :oldMethod|
		self methodDict 
			at: selector
			ifPresent: [ :currentMethod|
				currentMethod = oldMethod
					ifFalse: [ self noteMethodChanged: oldMethod to: currentMethod]]
			ifAbsent: [ self noteMethodRemoved: oldMethod ]].
	