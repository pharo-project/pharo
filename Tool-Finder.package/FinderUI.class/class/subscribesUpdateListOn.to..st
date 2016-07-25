event subscriptions
subscribesUpdateListOn: aFinder to: anInstance
	aFinder
		when: #updateResultDictionary
		send: #updateList
		to: anInstance.