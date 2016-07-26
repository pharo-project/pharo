manifest
isFalsePositive: anItem onRule: ruleId version: versionId
	
	^ ((self containsFalsePositive: anItem onRule: ruleId version: versionId) or:
			[(self containsRejectedRule: ruleId) or:
				[(anItem class = RPackage) not and: [self containsRejectedClass: anItem criticClass]]])