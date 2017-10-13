initialization
bindingOf: aName
"	(ringMethod ast hasTemporaryNamed: aName)
		ifTrue: [ ^ nil ].

	(ringMethod ast hasArgumentNamed: aName)
		ifTrue: [ ^ nil ].

	"
	^ (ringClass bindingOf: aName)"
	binding isNil] whileTrue: [
		 ]

	ringClass environment at: aName ifPresent: [ :aClass | ^ GlobalVariable key: aName value: aClass ] ifAbsent: [  ].

	self halt"