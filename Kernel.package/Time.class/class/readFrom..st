instance creation
readFrom: aStream
	"Read a Time from the stream in the form:
		<hour>:<minute>:<second>.<nseconds> <am/pm>
	<minute>, <second> or <am/pm> may be omitted.  e.g. 1:59:30 pm; 8AM; 15:30"

	| hour minute second ampm nanos power |
	hour := Integer readFrom: aStream.
	minute := second := nanos := 0.
	(aStream peekFor: $:) 
		ifTrue: [
			minute := Integer readFrom: aStream.
			(aStream peekFor: $:) 
				ifTrue: [
					second := Integer readFrom: aStream.
					(aStream peekFor: $.)
						ifTrue: [ 
							power := 1.
							[ aStream atEnd not and: [ aStream peek isDigit ] ]
								whileTrue: [  
									nanos := nanos * 10 + aStream next digitValue.
									power := power * 10 ].
							nanos := nanos / power * 1000000000 ] ] ].
	aStream skipSeparators.
	(aStream atEnd not and: [ 'APap' includes: aStream peek ]) 
		ifTrue: [
			ampm := aStream next asLowercase.
			(ampm = $p and: [ hour < 12 ]) ifTrue: [ hour := hour + 12 ].
			(ampm = $a and: [ hour = 12 ]) ifTrue: [ hour := 0 ].
			(aStream peekFor: $m) ifFalse: [ aStream peekFor: $M ] ].
	^ self 
		hour: hour 
		minute: minute 
		second: second 
		nanoSecond: nanos