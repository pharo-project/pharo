Extension { #name : #UIManager }

{ #category : #'*Regex-Core' }
UIManager >> request: aTitleString regex: initialRegexString [
	"Prompt the user for a valid regex.
	 Return nil on cancel or a valid RxMatcher"
	| regex |
	regex := initialRegexString.
	"loop until we get a valid regex string back"
	[
		regex := UIManager default 
			multiLineRequest: aTitleString 
			initialAnswer: regex 
			answerHeight: 200.
		"cancelled dialog ==> nil"	
		regex ifNil: [ ^ nil ].
		
		[ ^ regex asRegex ] on: Error do: [ :regexParsingError|
			self defer: [	self inform: 'Bad Regex: ', regexParsingError asString ]].
	] repeat
]
