private
computeSelectorCacheFor: aClass 
	^(self instanceVariableSelectorsFor: aClass)
		addAll: (self classVariableSelectorsFor: aClass);
		yourself