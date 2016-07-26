event subscriptions
subscribesUpdateSelectedMethodOn: aFinder to: anInstance
	aFinder
		when: #updateSelectedMethod
		send: #updateSelectedMethod
		to: anInstance.