checking
checkAllClasses

	| warnings |
	self checkBasicClasses.

	warnings := ((self systemNavigation allClassesImplementing: #veryDeepInner:),
		(self systemNavigation allClassesImplementing: #veryDeepCopyWith:)) flatCollect: [ :aClass |
			 self basicCheckClass: aClass ].
	
	self raiseWarningsIfAny: warnings