*Multilingual-OtherLanguages
isUnifiedKanji: code
	^ ((((code between: 16r2E80 and: 16rA4CF)
		or: [ code between: 16rF900 and: 16rFAFF ])
			or: [ code between: 16rFE30 and: 16rFE4F ]) 
				or: [ code between: 16rFF00 and: 16rFFEF ])
					or: [ code between: 16r20000 and: 16r2FA1F ]