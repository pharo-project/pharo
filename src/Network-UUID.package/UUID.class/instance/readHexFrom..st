private
readHexFrom: aStream
	| map v |
	map := '0123456789abcdef'.
	v := (map indexOf: aStream next asLowercase ifAbsent: [ self error: 'hex digit expected' ]) - 1.
	v := v bitShift: 4.
	v := v + (map indexOf: aStream next asLowercase ifAbsent: [ self error: 'hex digit expected' ]) - 1.
	^ v