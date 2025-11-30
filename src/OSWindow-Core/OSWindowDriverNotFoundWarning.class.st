"
Informs the defined PHARO_WINDOW_DRIVER was not found. 
This is a warning because it does not stops the system, just raises a traceable notification.
"
Class {
	#name : 'OSWindowDriverNotFoundWarning',
	#superclass : 'Warning',
	#instVars : [
		'driverName'
	],
	#category : 'OSWindow-Core-Utilities',
	#package : 'OSWindow-Core',
	#tag : 'Utilities'
}

{ #category : 'handling' }
OSWindowDriverNotFoundWarning >> defaultAction [

	InformativeNotification signal: self messageText.
	self resume: nil
]

{ #category : 'accessing' }
OSWindowDriverNotFoundWarning >> driverName [
 
	^ driverName
]

{ #category : 'accessing' }
OSWindowDriverNotFoundWarning >> driverName: aString [
 
	driverName := aString
]

{ #category : 'accessing' }
OSWindowDriverNotFoundWarning >> messageText [

	^ 'PHARO_WINDOW_DRIVER=', self driverName, ' is not present. The default window driver will be used.'
]

{ #category : 'signaling' }
OSWindowDriverNotFoundWarning >> signal: aString [

	self driverName: aString.
	self signal
]
