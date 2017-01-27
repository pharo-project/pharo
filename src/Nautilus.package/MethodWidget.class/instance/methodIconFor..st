icon
methodIconFor: aMethod

	| actions button action |
	MethodsIconsCache
		at: aMethod
		ifPresent: [:methodIcon | 
			methodIcon isArray
				ifTrue: [ ^ self rebuildUpAndDownArrowIconFrom: methodIcon].
			methodIcon class == IconicButtonStateHolder
				ifTrue: [
					"convert the IconicButtonStateHolder previously cached by
					this method with #actionStateToCache before returning"
					^ methodIcon asIcon ].
			^ methodIcon. ].

	actions := AbstractMethodIconAction allSubclasses collect: [:class | class for: aMethod in: self model ].
	actions sort: [:a :b | a actionOrder < b actionOrder ].
	action := actions 
		detect: [:each | each isActionHandled ]. 
	
	"actionIcon need to be invoked before actionStateToCache"
	button := action actionIcon.
	MethodsIconsCache at: aMethod put: action actionStateToCache.
	^ button