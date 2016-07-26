initialize-release
on: aDictionary 
	aDictionary keysAndValuesDo: [ :class :selectors | 
		class isMeta 
			ifTrue: [ metaClassSelectors at: class soleInstance name put: selectors asIdentitySet ]
			ifFalse: [ classSelectors at: class name put: selectors asIdentitySet ] ]