printing
storeOn: arg1
	| tmp1 |
	self shouldBePrintedAsLiteral
		ifTrue: [ | tmp3 |
			tmp3 := arg1.
			tmp3
				nextPut: $$;
				nextPut: self ]
		ifFalse: [ tmp1 := self class constantNameFor: self.
			tmp1 notNil
				ifTrue: [ | tmp5 |
					tmp5 := arg1.
					tmp5
						nextPutAll: self class name;
						space;
						nextPutAll: tmp1 ]
				ifFalse: [ | tmp7 |
					tmp7 := arg1.
					tmp7
						nextPut: $(;
						nextPutAll: self class name;
						nextPutAll: ' value: ';
						print: self asInteger;
						nextPut: $) ] ]