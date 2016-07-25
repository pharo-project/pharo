adding
add: preSelectors requiresPostSend: postSelectors

	postConditions add: (
		ReInvocationSequenceRuleSequence
			before: preSelectors
			after: postSelectors)