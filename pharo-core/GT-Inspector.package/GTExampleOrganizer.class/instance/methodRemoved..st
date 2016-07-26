private-events
methodRemoved: anAnnouncement
	self withoutExceptionsDo: [ 
		((examples at: anAnnouncement method methodClass theNonMetaClass name ifAbsent: [ IdentityDictionary new ])
			removeKey: anAnnouncement method selector ifAbsent: [ nil ]) ifNotNil: [ :example |
				[ example detachFromSystem ] on: Error do: [ " ignore " ].
				"self example: example removed: anAnnouncement."
				self checkForDependenciesUsing: anAnnouncement method.
				GTExampleNautilus methodChanged: anAnnouncement method.  ] ]