comparing
= anMCDependentsWrapper

	self class = anMCDependentsWrapper class ifFalse: [^ false].
	^ (self item package name = anMCDependentsWrapper item package name) and: [self model = anMCDependentsWrapper model].
