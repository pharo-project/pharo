"
I am a notification that informs user aboout some important operation that the system did. 
"
Class {
	#name : 'SystemNotification',
	#superclass : 'Notification',
	#category : 'Kernel-Exceptions',
	#package : 'Kernel',
	#tag : 'Exceptions'
}

{ #category : 'handling' }
SystemNotification >> defaultAction [

	^ Processor activeProcess handleNotification: self
]
