"
I have the simple task of printing a date, time and DateTime.
I do it in the simplest way possible, my subclasses can extend me, and I will use them.

I only print in a single format, does not matter what format the parameter is.

I use ISO 8601 for formatting Date, Time and DateTime

https://en.wikipedia.org/wiki/ISO_8601
"
Class {
	#name : 'BasicDatePrinter',
	#superclass : 'Object',
	#category : 'Deprecated12',
	#package : 'Deprecated12'
}

{ #category : 'accessing' }
BasicDatePrinter class >> default [
	^ ExtendedDatePrinter new
]

{ #category : 'testing' }
BasicDatePrinter class >> isDeprecated [

	^ true
]

{ #category : 'printing' }
BasicDatePrinter >> printDate: aDate format: formatArray on: aStream [
	"I print a basic representation of the date in ISO 8601 ex: 	2018-03-08"

	self printYMD: aDate withLeadingSpace: false on: aStream
]

{ #category : 'printing' }
BasicDatePrinter >> printDateAndTime: aDateTime withLeadingSpace: printLeadingSpaceToo on: aStream [

	self
		deprecated: 'Just use DateAndTime>>printOn:withLeadingSpace:'
		transformWith: '`@rcv printDateAndTime: `@date withLeadingSpace: `@bool on: `@stream' -> '`@date printOn: `@stream withLeadingSpace: `@bool'.

	aDateTime printOn: aStream withLeadingSpace: printLeadingSpaceToo
]

{ #category : 'printing' }
BasicDatePrinter >> printHMS: aTime separatedBy: aSeparator on: aStream [

	self
		deprecated: 'Just use Time>>printHMSOn:separatedBy:'
		transformWith: '`@rcv printHMS: `@time separatedBy: `@sep on: `@stream' -> '`@time printHMSOn: `@stream separatedBy: `@sep'.

	aTime printHMSOn: aStream separatedBy: aSeparator
]

{ #category : 'printing' }
BasicDatePrinter >> printYMD: aDate withLeadingSpace: printLeadingSpaceToo on: aStream [

	self
		deprecated: 'Just use Date>>printYMDOn:withLeadingSpace:'
		transformWith: '`@rcv printYMD: `@date withLeadingSpace: `@bool on: `@stream' -> '`@date printYMDOn: `@stream withLeadingSpace: `@bool'.
	^ aDate printYMDOn: aStream withLeadingSpace: printLeadingSpaceToo
]
