protocol
replaceAll: oldMethod by: newMethod

	critics valuesDo: [:each |  
		(each includes: oldMethod)
			ifTrue: [
				each remove: oldMethod.
				each add: newMethod ]].
		
	falsePositives valuesDo: [:each |  
		(each includes: oldMethod)
			ifTrue: [
				each remove: oldMethod.
				each add: newMethod ]].

	toDos valuesDo: [:each |  
		(each includes: oldMethod)
			ifTrue: [
				each remove: oldMethod.
				each add: newMethod ]]
	