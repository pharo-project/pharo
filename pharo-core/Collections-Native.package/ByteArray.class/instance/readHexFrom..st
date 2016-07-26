initialize-release
readHexFrom: aStream
	"Initialize the receiver from a hexadecimal string representation"
	| map v ch value |
	map := '0123456789abcdefABCDEF'.
	1 to: self size do: [ :i |
		ch := aStream next.
		v := (map indexOf: ch) - 1.
		((v between: 0 and: 15) or: [((v:= v - 6) between: 0 and: 15)]) ifFalse:[^self error: 'Hex digit 
expected'].
		value := v bitShift: 4.
		ch := aStream next.
		v := (map indexOf: ch) - 1.
		((v between: 0 and: 15) or: [((v:= v - 6) between: 0 and: 15)]) ifFalse:[^self error: 'Hex digit 
expected'].
		value := value + v.
		self at: i put: value].