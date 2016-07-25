event subscriptions
subscribesEnableUseRegExOn: aFinder to: anInstance
	aFinder
		when: #enableUseRegEx
		send: #enableUseRegEx
		to: anInstance.