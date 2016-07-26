tests
testRemoveTraitFromComposition

	aClass := classFactory newClassUsing: TSizeTest.

	self 
		shouldnt: [ 
			aClass superclass 
				subclass: aClass name 
				uses: {} asTraitComposition 
				instanceVariableNames: ''
				classVariableNames: ''
				category: aClass category. ] 
		raise: MessageNotUnderstood
	