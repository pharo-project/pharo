adding
addClass: aClass 
	aClass isMeta 
		ifTrue: [ metaClassSelectors at: aClass soleInstance name put: aClass selectors asIdentitySet ]
		ifFalse: [ classSelectors at: aClass name put: aClass selectors asIdentitySet ]