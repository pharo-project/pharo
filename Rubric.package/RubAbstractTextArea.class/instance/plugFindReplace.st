initialize
plugFindReplace
	self announcer 
		when:  MorphGotFocus send: #whenTextAreaGotFocus: to: RubFindReplaceService default;
	 	when:  MorphLostFocus send: #whenTextAreaLostFocus: to: RubFindReplaceService default;	
		when:  RubSelectionChanged send: #whenTextAreaSelectionChanged: to: RubFindReplaceService default