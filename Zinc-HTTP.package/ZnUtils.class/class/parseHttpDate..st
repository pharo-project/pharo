conversions
parseHttpDate: string
	"self parseHttpDate: 'Tue, 13 Sep 2011 08:04:49 GMT'."
	"self parseHttpDate: 'Tue, 13-Sep-2011 08:04:51 GMT'."
	"self parseHttpDate: 'Tue Jan 01 00:00:01 2036 GMT'."
	
	| tokens day month year hour minute second months map yearToken |
	tokens := (string findTokens: #( $ $- $: $, )) allButFirst.
	tokens last = 'GMT' ifTrue: [ tokens := tokens allButLast ].
	months := #(jan feb mar apr may jun jul aug sep oct nov dec).
	(Integer readFrom: tokens first ifFail: [ 0 ]) isZero
		ifTrue: [ map := #(2 1 6 3 4 5) ]
		ifFalse: [ map := #(1 2 3 4 5 6) ].
	day := Integer readFrom: (tokens at: map first) ifFail: [ 0 ].
	month := months indexOf: (((tokens at: map second) asLowercase) copyFrom: 1 to: 3) ifAbsent: [ 0 ].
	yearToken := tokens at: map third.
	year := yearToken isAllDigits
		ifTrue: [
			year := yearToken asNumber.
			yearToken size = 2
				ifTrue: [
					year < 50
						ifTrue: [ year + 2000 ]
						ifFalse: [ year + 1900 ] ]
				ifFalse: [ year ] ]
		ifFalse: [ 0 ].
	hour := Integer readFrom: (tokens at: map fourth) ifFail: [ 0 ].
	minute := Integer readFrom: (tokens at: map fifth) ifFail: [ 0 ].
	second := Integer readFrom: (tokens at: map sixth) ifFail: [ 0 ].
	^ DateAndTime year: year month: month day: day hour: hour minute: minute second: second offset: Duration zero