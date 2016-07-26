other
isDefault
	"Return true if the receiver is a default and can be omitted"
	locked == true
		ifTrue: [^ false].
	visible == false
		ifTrue: [^ false].
	sticky == true
		ifTrue: [^ false].
	balloonText ifNotNil: [^ false].
	externalName ifNotNil: [^ false].
	eventHandler ifNotNil: [^ false].
	otherProperties ifNotNil: [otherProperties isEmpty ifFalse: [^ false]].
	^ true