initialization
initialize
	super initialize.
		
	self headerBrick: self newHeaderBrick.
	"#GTSpotterEventRecorderSettings asClassIfPresent: [ 
		self privacyAgreementBrick: self newPrivacyAgreementBrick ]."