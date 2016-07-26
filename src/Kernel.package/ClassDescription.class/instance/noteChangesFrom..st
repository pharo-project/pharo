organization updating
noteChangesFrom: oldMethodDict
	"create notifications about the changes made to the method dictionary"
	"additions have already been notified in #addTraitSelector:withMethod:"
		
	" deal with removal / updates relative to the old method dictionary"
	oldMethodDict keysAndValuesDo: [ :selector :oldMethod|
		self methodDict 
			at: selector
			ifPresent: [ :currentMethod|
				currentMethod == oldMethod
					ifFalse: [ self noteMethodChanged: oldMethod to: currentMethod]]
			ifAbsent: [ self noteMethodRemoved: oldMethod ]].
	