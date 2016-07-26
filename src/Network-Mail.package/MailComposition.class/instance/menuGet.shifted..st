interface
menuGet: aMenu shifted: shifted
	
	aMenu addList: {
		{'Find...(f)' .	#find}.
		{'Find again (g)' . #findAgain}.
		{'Set search string (h)' . #setSearchString}.
		#-.
		{'Accept (s)' . #accept}.
		{'Send message' .  #submit}}.
	^aMenu.