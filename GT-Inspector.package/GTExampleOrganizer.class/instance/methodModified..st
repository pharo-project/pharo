private-events
methodModified: anAnnouncement
	self withoutExceptionsDo: [ 
		((examples at: anAnnouncement oldMethod methodClass theNonMetaClass name ifAbsent: [ IdentityDictionary new ])
			removeKey: anAnnouncement oldMethod selector ifAbsent: [ nil ]) ifNotNil: [ :example |
				[ example detachFromSystem ] on: Error do: [ " ignore " ].
				"self example: example modified: anAnnouncement."
				self checkForMissingDependencies: anAnnouncement method.
				self checkForNumberOfArguments: anAnnouncement method.
				self checkForMissingClasses: anAnnouncement method.
				self checkForDependenciesUsing: anAnnouncement method.
				GTExampleNautilus methodChanged: anAnnouncement method.
				GTExampleNautilus methodChanged: anAnnouncement oldMethod ] ]