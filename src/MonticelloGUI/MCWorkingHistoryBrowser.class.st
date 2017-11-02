"
Browser for working history
"
Class {
	#name : #MCWorkingHistoryBrowser,
	#superclass : #MCVersionHistoryBrowser,
	#category : 'MonticelloGUI'
}

{ #category : #accessing }
MCWorkingHistoryBrowser >> baseSnapshot [
	^ package snapshot
]
