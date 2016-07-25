accessing
direction

	^direction ifNil: [direction :=( (normal y @ normal x negated) * form width / form height ) rounded]