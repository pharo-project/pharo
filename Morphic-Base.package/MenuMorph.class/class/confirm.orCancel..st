private - utilities
confirm: queryString orCancel: cancelBlock 
	"Put up a yes/no/cancel menu with caption aString. Answer true if  the response is yes, false if no. If cancel is chosen, evaluate cancelBlock. This is a modal question--the user must respond yes or no.  Do not invoke this method - it is here to help in case of mini system, better use UIManager default"
	"MenuMorph confirm: 'Reboot universe now' orCancel:[^'Nevermind'] "
	
	| choice |
	choice := self chooseFrom: {'Yes' translated. 'No' translated. 'Cancel' translated}
		lines: #()
		title: queryString.
	choice = 1 ifTrue: [^ true].
	choice = 2 ifTrue: [^ false].
	^ cancelBlock value