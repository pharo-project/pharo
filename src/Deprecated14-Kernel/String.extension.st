Extension { #name : 'String' }

{ #category : '*Deprecated14-Kernel' }
String >> asDate [
	"Read a date in one of the numeric forms Date class>>#readFrom: used to accept: 1982-04-15, 4/15/1982, 15/4/1982, 15.4.1982, 4/15/82. As it did, a month that can also be a day comes first (4/5/1982 is April 5th), and a two-digit year means 2000 to 2068 below 69 and 1969 to 1999 from 69 on.

	Limitations: unlike Date class>>#readFrom:, it does not read month names (15 April 1982, 15-APR-82, April 15, 1982) nor negative years, since Date class>>#readFrom:pattern: does not. Prefer Date class>>#readFrom:pattern: with the one pattern of your dates."

	self deprecated: 'Use Date>>#readFrom:pattern: specifying a concrete pattern instead.' in: #Pharo14.
	"Not Date class>>#readFrom:, whose compatibility period ends before this one"
	#( 'yyyy-m-d' 'm/d/yyyy' 'd/m/yyyy' 'm-d-yyyy' 'd-m-yyyy' 'd.m.yyyy' ) do: [ :pattern |
		[ ^ Date readFrom: self pattern: pattern ]
			on: DateError
			do: [ :error | "try the next pattern" ] ].
	#( 'm/d/yy' 'd/m/yy' 'm-d-yy' 'd-m-yy' 'd.m.yy' ) do: [ :pattern |
		[
			| date |
			date := Date readFrom: self pattern: pattern.
			"The pattern reads 2000 to 2099"
			^ date year >= 2069
				  ifTrue: [ Date year: date year - 100 month: date monthIndex day: date dayOfMonth ]
				  ifFalse: [ date ] ]
			on: DateError
			do: [ :error | "try the next pattern" ] ].
	^ DateError signal: 'Not a date in a known form: ' , self
]

{ #category : '*Deprecated14-Kernel' }
String >> trimmed [
	"Trim separators from both sides of the receiving string."

	self
		backwardCompatible: 'Use trimBoth instead -> https://github.com/pharo-project/pharo/issues/18307'
		on: '28 June 2025'
		in: 'pharo14'
		transformWith: '`@receiver trimmed' -> '`@receiver trimBoth'.


	^ self trimBoth
]
