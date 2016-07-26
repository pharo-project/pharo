as yet unclassified
testPrivacyChanged3
	"Ensure that delivery is turn on when privacy is available and there is a collector."
	self testPrivacyChanged.
	recorder addCollector: collector1.
	self deny: recorder canSendDiagnosticsAndUsageData.
	self deny: recorder delivery hasPermission.
	recorder offerPrivacy.
	self assert: recorder canSendDiagnosticsAndUsageData.
	self deny: recorder delivery hasPermission.
	recorder privacyChanged.
	self assert: recorder canSendDiagnosticsAndUsageData.
	self assert: recorder delivery hasPermission.
