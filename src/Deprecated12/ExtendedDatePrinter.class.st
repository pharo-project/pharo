"
I extend the behavior of BasicDatePrinter with the whole format supported in Pharo
"
Class {
	#name : 'ExtendedDatePrinter',
	#superclass : 'BasicDatePrinter',
	#category : 'Deprecated12',
	#package : 'Deprecated12'
}

{ #category : 'printing' }
ExtendedDatePrinter >> printDate: aDate format: formatArray on: aStream [

	self
		deprecated: 'Just use Date>>printOn:format:'
		transformWith: '`@rcv printDate: `@date format: `@format on: `@stream' -> '`@date printOn: `@stream format: `@format'.
	^ aDate printOn: aStream format: formatArray
]
