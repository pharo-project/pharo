accessing
chooseRecentClipping  
	"Choose by menu from among the recent clippings"
	"Clipboard chooseRecentClipping"
	
	recent ifNil: [^ nil].
	^ UIManager default
		chooseFrom: (recent collect: [:txt | ((txt asString contractTo: 50)
									copyReplaceAll: Character cr asString with: '\')
									copyReplaceAll: Character tab asString with: '|'])
		values: recent