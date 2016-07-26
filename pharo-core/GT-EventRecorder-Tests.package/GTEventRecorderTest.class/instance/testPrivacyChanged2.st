as yet unclassified
testPrivacyChanged2
	"Ensure that delivery is turn off when privacy is available, but there is no any collector."
	self testPrivacyChanged.
	recorder offerPrivacy.
	self assert: recorder canSendDiagnosticsAndUsageData.
	self deny: recorder delivery hasPermission.
	recorder privacyChanged.
	self assert: recorder canSendDiagnosticsAndUsageData.
	self deny: recorder delivery hasPermission.
