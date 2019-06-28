Extension { #name : #TestRaisedWarning }

{ #category : #'*SUnit-UI' }
TestRaisedWarning >> color [
	"Same color as failure to be retro-compatible."
	^ self theme warningBackgroundColor
]
