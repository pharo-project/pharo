accessing-computed
currentVersion
	"Return the currentVersion of the configuration ... nil if not loaded."

	currentVersion == #notLoaded
		ifTrue: [ ^ nil ].
	(currentVersion notNil and: [ currentVersion ~~ #recalculate])
		ifTrue: [ ^ currentVersion ].
	packages := projects := groups := text := nil.
	self configurationRoot ifNil: [  ^ currentVersion ].
	(currentVersion := self project currentVersion)
		ifNil: [ 
			currentVersion := #notLoaded.
			self configurationRoot announcer announce: (MBConfigurationInfoChanged changed: self).
			^ nil ].
	self configurationRoot announcer announce: (MBConfigurationInfoChanged changed: self).
	^ currentVersion