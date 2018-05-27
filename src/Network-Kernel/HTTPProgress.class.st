"
I am HTTPProgress, a notification to show progress when using HTTP.

I include
	- total: The total size of the download/upload (if known)
	- amount: The completed amount of the download/upload (if known)

Use #total:, #amount: or #amountLeft: to set the appropriate byte counts to indicate progress.
Use #fraction or #percentage as a value that indicates progress.
Total and amount are optional and can be nil. Test using #isEmpty.

You can try 
	HTTPProgress example.

"
Class {
	#name : #HTTPProgress,
	#superclass : #Notification,
	#instVars : [
		'total',
		'amount'
	],
	#category : #'Network-Kernel-Notifications'
}

{ #category : #examples }
HTTPProgress class >> example [
	"self example"
	
 	UIManager default informUserDuring: [ :bar |
		bar label: 'Transfer Demo...'.
		1 second asDelay wait.
		[ 1 to: 20 do: [ :each |
			HTTPProgress signal: 'Transferring...' amount: each * 1024 total: 20*1024.
			(Delay forMilliseconds: 100) wait ] ]
			on: HTTPProgress 
			do: [ :progress | 
				bar label: progress printString.
				progress isEmpty ifFalse: [ bar current: progress percentage ]. 
				progress resume ] ] 
]

{ #category : #exceptioninstantiator }
HTTPProgress class >> signal: signalerText amount: amount total: total [
	"Create and signal HTTPProgress with amount bytes transferred out of total.
	Use an additional signalerText."

	^ self new
		amount: amount;
		total: total;
		signal: signalerText
]

{ #category : #exceptioninstantiator }
HTTPProgress class >> signalAmount: amount total: total [
	"Create and signal HTTPProgress with amount bytes transferred out of total."
	
	^ self new
		amount: amount;
		total: total;
		signal
]

{ #category : #accessing }
HTTPProgress >> amount [
	"Answer the amount that has already been transferred.
	Can be nil. Should be between 0 and total."

	^ amount
]

{ #category : #accessing }
HTTPProgress >> amount: byteCount [
	"Set the amount of bytes that has already been transferred."

	amount := byteCount
]

{ #category : #accessing }
HTTPProgress >> amountLeft [
	"Answer the amount that has not yet been transferred.
	Can be nil. Should be between 0 and total."

	^ self isEmpty ifFalse: [ total - amount ]
]

{ #category : #accessing }
HTTPProgress >> amountLeft: byteCount [
	"Set the amount that has not yet been transferred.
	Can be nil. Should be between 0 and total."

	^ total ifNotNil: [ amount := total - byteCount ]
]

{ #category : #accessing }
HTTPProgress >> beComplete [
	"Make me complete, i.e. indicate that all bytes were transferred."

	amount := total
]

{ #category : #accessing }
HTTPProgress >> fraction [
	"Answer the fraction of total that has already been transferred.
	Can be nil. Should be between 0 and 1."
	
	^ self isEmpty ifFalse: [ amount / total ]
]

{ #category : #testing }
HTTPProgress >> isComplete [
	"Answer true when I am complete, i.e. all bytes were transferred.
	When I am empty, I am also complete."

	^ amount = total
]

{ #category : #testing }
HTTPProgress >> isEmpty [
	"Answer true if I do not contain a numerical progress indication."
  	
	^ amount isNil or: [ total isNil ]
]

{ #category : #accessing }
HTTPProgress >> percentage [
	"Answer the percentage of total that has already been transferred.
	Can be nil. Should be between 0 and 100."

	^ self isEmpty ifFalse: [ self fraction * 100 ]
]

{ #category : #printing }
HTTPProgress >> printOn: stream [
	"Print an extra progress percentage if available"
	
	super printOn: stream.
	self isEmpty 
		ifFalse: [ 
			stream space; print: self percentage rounded; nextPut: $% ]
]

{ #category : #accessing }
HTTPProgress >> total [
	"Answer the total byte count to transfer. Can be nil."

	^ total
]

{ #category : #accessing }
HTTPProgress >> total: byteCount [
	"Set the total byte count to transfer"

	total := byteCount
]
