testing
isContinuing
	^ self continuing 
		and: [ self streamed isEmptyOrNil not 
			and: [ self canContinue ] ]