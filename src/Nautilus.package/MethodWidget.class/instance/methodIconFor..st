icon
methodIconFor: aMethod

	| actions button action |
	MethodsIconsCache
		at: aMethod
		ifPresent: [:icon | 
			icon isArray ifTrue: [ ^ self rebuildUpAndDownArrowIconFrom: icon].
			icon class == IconicButtonStateHolder ifFalse: [ ^ icon ].
			^ icon asIcon ].

	actions := AbstractMethodIconAction allSubclasses collect: [:class | class for: aMethod in: NautilusUI ].
	actions sort: [:a :b | a actionOrder < b actionOrder ].
	action := actions 
		detect: [:each | each isActionHandled ]. 
	
	"actionIcon need to be invoked before actionStateToCache"
	button := action actionIcon.
	MethodsIconsCache at: aMethod put: action actionStateToCache.
	^ button