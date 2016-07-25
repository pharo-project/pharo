notifying
notifyObservers
	SystemAnnouncer uniqueInstance 
		suspendAllWhile: [myClass organization classify: selector under: category].
	priorMethodOrNil 
		ifNil: [ SystemAnnouncer uniqueInstance methodAdded: compiledMethod ]
		ifNotNil: [
			SystemAnnouncer uniqueInstance methodChangedFrom: priorMethodOrNil to: compiledMethod oldProtocol: priorCategoryOrNil.
			priorCategoryOrNil = category ifFalse: [
       			SystemAnnouncer uniqueInstance methodRecategorized: compiledMethod oldProtocol: priorCategoryOrNil ] ].
	"The following code doesn't seem to do anything."
	myClass instanceSide noteCompilationOfMethod: compiledMethod meta: myClass isClassSide.
