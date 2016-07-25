critic creation
postCriticFor: ast at: node regarding: sequence

	^ self critic: ReLaterInvocationCritique for: ast at: node regarding: sequence 