handling
resume: resumptionValue
	"Return resumptionValue as the value of the signal message."

	self isResumable ifFalse: [ IllegalResumeAttempt signal ].
	self resumeUnchecked: resumptionValue