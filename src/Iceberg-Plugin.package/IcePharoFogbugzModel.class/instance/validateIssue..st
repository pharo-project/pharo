private
validateIssue: aString
	| number |

	aString size = 5 ifFalse: [ ^ self ].
	aString isAllDigits ifFalse: [ ^ self ].
	number := aString asNumber.
	
	(issue isNil or: [ issue number ~= number ])
		ifTrue: [ 
			issue := PharoIssue number: number.
			[ 	"Fork to allow UI to continue"
				issue downloadTitle.
				World defer: [ self updateText: (self sanitizeTitle: issue title) ] ] fork ]
	