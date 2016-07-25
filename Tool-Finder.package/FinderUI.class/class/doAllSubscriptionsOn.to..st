event subscriptions
doAllSubscriptionsOn: aFinder to: anInstance
	self subscribesUpdateSourceCodeOn: aFinder to: anInstance.
	self subscribesResetEnvironmentOn: aFinder to: anInstance.
	self subscribesEnableUseRegExOn: aFinder to: anInstance.
	self subscribesDisableUseRegExOn: aFinder to: anInstance.
	self subscribesUpdateSelectedMethodOn: aFinder to: anInstance.
	self subscribesUpdateSelectedClassOn: aFinder to: anInstance.
	self subscribesUpdateListOn: aFinder to: anInstance