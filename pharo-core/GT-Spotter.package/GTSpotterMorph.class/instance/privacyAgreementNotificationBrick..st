accessing
privacyAgreementNotificationBrick: aBrick
	
	privacyAgreementNotificationBrick ifNotNil: [self removeBrick: privacyAgreementNotificationBrick].
	privacyAgreementNotificationBrick := aBrick.
	
	self themer spotterThemer privacyAgreementNotificationStyleFor: privacyAgreementNotificationBrick.
	privacyAgreementNotificationBrick layout widthDependency: self.
	privacyAgreementNotificationBrick width: [ self width - (self paneBrick isPreviewShown 
			ifFalse: [self themer spotterThemer previewArrowWidth - (2*self themer spotterThemer previewBorderWidth)] 
			ifTrue: [0])].
	
	privacyAgreementNotificationBrick settingsButtonBrick when: #onClicked send: #closeSpotter to: self.
	self addBrickBack: privacyAgreementNotificationBrick