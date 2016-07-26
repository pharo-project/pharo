private - preferences
isChangingPreferences

	^ self isOverridingPreferences or: [ self isOmittingPreferences ]