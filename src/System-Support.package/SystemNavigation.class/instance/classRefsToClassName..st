private
classRefsToClassName: aClassName
	| global |
	global := self environment at: aClassName ifAbsent: [ nil ].
	^ global isBehavior
		ifTrue: [ ^ self allReferencesTo: global binding ]
		ifFalse: [ #() ]