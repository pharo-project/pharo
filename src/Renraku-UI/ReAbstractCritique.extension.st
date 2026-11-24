Extension { #name : 'ReAbstractCritique' }

{ #category : '*Renraku-UI' }
ReAbstractCritique >> actions [

	| actions |

	actions := OrderedCollection new: 3.

	actions add: (RePropertyAction new
			icon: (self iconNamed: #smallQuestion);
			description: 'View rationale behind the rule';
			action: [ :crit |
				crit popDescriptionUp.
				ReSystemAnnouncer uniqueInstance
					notifyCritique: crit
					descriptionViewedFor: crit sourceAnchor entity ];
			yourself).

	self providesChange ifTrue: [
		actions add: (RePropertyAction new
			icon: (self iconNamed: #repair);
			description: 'Automatically resolve the issue';
			action: [ :crit |
				| changesBrowser |

				changesBrowser := (StChangesBrowserPresenter changes: { crit change })
					extent: 500@400;
					asModalWindow;
					yourself.

				changesBrowser openDialog
					okAction: [ changesBrowser accept ];
					centered ];
			yourself) ].

	actions add: (RePropertyAction new
			icon: (self iconNamed: #smallCancel);
			description: 'Ban this rule';
			action: [ :crit | crit guidedBan ];
			yourself).

	^ actions
]

{ #category : '*Renraku-UI' }
ReAbstractCritique >> popDescriptionUp [

	MorphicUIManager new longMessage: self description title: self title
]
