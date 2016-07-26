parsing-private
readNamedFloatOrFail
	"This method is used when there is no digit encountered:
	It try and read a named Float NaN or Infinity.
	Negative sign for -Infinity has been read before sending this method, and is indicated in the neg inst.var.
	Fail if no named Float is found"
		
	neg ifFalse: [(sourceStream nextMatchAll: 'NaN')
			ifTrue: [^ Float nan]].
	(sourceStream nextMatchAll: 'Infinity')
		ifTrue: [^ neg
			ifTrue: [Float infinity negated]
			ifFalse: [Float infinity]].
	^self expected: 'a digit between 0 and ' , (String with: (Character digitValue: base - 1))