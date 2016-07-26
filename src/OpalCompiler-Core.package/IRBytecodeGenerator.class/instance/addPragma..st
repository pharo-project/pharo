private
addPragma: aPragma
	properties ifNil: [ properties := AdditionalMethodState new ].
	properties := properties copyWith: aPragma. 

	
	