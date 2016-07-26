as yet unclassified
testPrivacyChanged
	"Ensure that delivery is turn off when privacy is disabled."
	self assert: recorder canSendDiagnosticsAndUsageData.
	self assert: recorder delivery hasPermission.
	recorder withdrawPrivacy.
	self deny: recorder canSendDiagnosticsAndUsageData.
	self assert: recorder delivery hasPermission.
	recorder privacyChanged.
	self deny: recorder canSendDiagnosticsAndUsageData.
	self deny: recorder delivery hasPermission.
