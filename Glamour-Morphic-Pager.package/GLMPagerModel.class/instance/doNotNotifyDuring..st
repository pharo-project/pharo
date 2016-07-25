as yet unclassified
doNotNotifyDuring: aBlock
	|allowed|
	allowed := self isNotificationsAllowed.
	isNotificationsAllowed := false.
	aBlock cull: self.
	allowed ifTrue: [ isNotificationsAllowed := true ]