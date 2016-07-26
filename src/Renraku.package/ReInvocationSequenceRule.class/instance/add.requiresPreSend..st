adding
add: postSelectors requiresPreSend: preSelectors

	preConditions add: (
		ReInvocationSequenceRuleSequence
			before: preSelectors
			after: postSelectors)