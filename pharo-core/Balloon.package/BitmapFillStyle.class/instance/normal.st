accessing
normal
	^normal ifNil: [normal := ((direction y negated @ direction x) *  form height / form width ) rounded]