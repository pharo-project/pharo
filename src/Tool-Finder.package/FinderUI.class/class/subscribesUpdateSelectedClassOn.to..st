event subscriptions
subscribesUpdateSelectedClassOn: aFinder to: anInstance
	aFinder
		when: #updateSelectedClass
		send: #updateSelectedClass
		to: anInstance.