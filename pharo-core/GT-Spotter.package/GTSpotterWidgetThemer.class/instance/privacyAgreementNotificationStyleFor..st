styles
privacyAgreementNotificationStyleFor: aBrick

	| color |
	color := aBrick sendUsageData 
		ifTrue: [ self privacyAgreementIsSendingDataColor. ]
		ifFalse: [ self privacyAgreementIsNotSendingDataColor. ].
	aBrick messageBrick rubricTextFamilyName: StandardFonts defaultFont familyName.
	aBrick messageBrick rubricTextColor: Color black.
	aBrick
		color: color;
		borderColor: color darker;
		borderWidth: self outerBorderWidth;
		marginTop: 0