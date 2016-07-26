accessing
isNotificationsAllowed

	^ isNotificationsAllowed ifNil: [ isNotificationsAllowed := true ]