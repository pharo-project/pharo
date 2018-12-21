"
Browser for working history
"
Class {
	#name : #MCWorkingHistoryBrowser,
	#superclass : #MCVersionHistoryBrowser,
	#category : #'MonticelloGUI-Tools'
}

{ #category : #accessing }
MCWorkingHistoryBrowser >> baseSnapshot [
	^ package snapshot
]
