transformations
superSends
	^(self new)
		name: 'Rewrite super messages to self messages when both refer to same method';
		superSends;
		yourself