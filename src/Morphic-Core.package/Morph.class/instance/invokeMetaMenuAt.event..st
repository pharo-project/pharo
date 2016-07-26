meta-actions
invokeMetaMenuAt: aPoint event: evt 
	| morphs target |
	morphs := self morphsAt: aPoint.
	(morphs includes: self)
		ifFalse: [morphs := morphs copyWith: self].
	morphs size = 1
		ifTrue: [morphs anyOne invokeMetaMenu: evt]
		ifFalse: [target := UIManager default
						chooseFrom: (morphs
								collect: [:t | t class name asString])
						values: morphs.
			target 
				ifNil: [^ self].
			target invokeMetaMenu: evt]