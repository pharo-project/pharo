private
criticalWarningReasonCodes
	^ super criticalWarningReasonCodes
		, #(#noLoadableVersions #noTests #testDeprecation #loadDeprecation #noVersionSpecified #'missingRecommendedProjectSpecField' )