actions
actions 

	| actions |
	
	actions := OrderedCollection new: 3.

	actions add: (RePropertyAction new
			icon: (Smalltalk ui icons iconNamed: #smallQuestionIcon);
			description: 'View rationale behind the rule';
			action: [ :crit |
				crit popDescriptionUp.
				ReSystemAnnouncer uniqueInstance
					notifyCritique: crit
					descriptionViewedFor: crit sourceAnchor entity ];
			yourself).
	
	self providesChange ifTrue: [ 
		actions add: (RePropertyAction new
			icon: (Smalltalk ui icons iconNamed: #repairIcon);
			description: 'Automatically resolve the issue';
			action: [ :crit |
				| changesBrowser |

				ReSystemAnnouncer uniqueInstance notifyViewedDiffFor: crit of: crit sourceAnchor entity.
				
				changesBrowser := (ChangesBrowser changes: { crit change })
					extent: 500@400;
					setModal: SystemWindow topWindow;
					yourself.
					
				changesBrowser openDialogWithSpec
					okAction: [ ReSystemAnnouncer uniqueInstance
								notifyCritique: crit
								AutoFixedFor: crit sourceAnchor entity.
								changesBrowser accept ];
					centered
				
				"(TextModel new
					title: 'Apply the proposed changes?';
					extent: 800@600;
					text: change diffedText;
					disable;
					openDialogWithSpec)
						okAction: [ ReSystemAnnouncer uniqueInstance
								notifyCritique: crit
								AutoFixedFor: crit sourceAnchor entity.
								change execute ];
			centered" ];
			yourself) ].
	
	actions add: (RePropertyAction new
			icon: (Smalltalk ui icons iconNamed: #smallCancelIcon);
			description: 'Ban this rule';
			action: [ :crit | crit guidedBan ];
			yourself).

	^ actions