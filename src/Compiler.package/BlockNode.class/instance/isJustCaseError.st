testing
isJustCaseError

	^ statements size = 1 and:
		[statements first
			isMessage: #caseError
			receiver: [:r | r==NodeSelf]
			arguments: nil]