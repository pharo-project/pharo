initialization
privacyMorph
	| agreementBrick |

	agreementBrick := GLMPrivacyAgreementBrick new.
	
	agreementBrick sendUsageData ifTrue: [ ^ listMorph ].
	
	agreementBrick messageBrick
		rubricTextFamilyName: StandardFonts defaultFont familyName.
	agreementBrick messageBrick rubricTextColor: Color black.
	agreementBrick
		vSpaceFill;
		color: Color paleRed.

	agreementBrick settingsButtonBrick
		when: #onClicked
		do: [
			showPrivacyMorph := false.
			self updateList ].
		
	^ agreementBrick asBrickedMorph
		hResizing: #spaceFill;
		vResizing: #spaceFill