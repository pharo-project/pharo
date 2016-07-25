private
digitCompare: arg 
	"Compare the magnitude of self with that of arg.   
	Return a code of 1, 0, -1 for self >, = , < arg"
	| len arglen argDigit selfDigit |
	<primitive: 'primDigitCompare' module:'LargeIntegers'>
	len := self digitLength.
	(arglen := arg digitLength) ~= len
		ifTrue: [arglen > len
				ifTrue: [^ -1]
				ifFalse: [^ 1]].
	[len > 0]
		whileTrue: 
			[(argDigit := arg digitAt: len) ~= (selfDigit := self digitAt: len)
				ifTrue: [argDigit < selfDigit
						ifTrue: [^ 1]
						ifFalse: [^ -1]].
			len := len - 1].
	^ 0