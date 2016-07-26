printing
printOn: arg1
	| tmp1 |
	(self asInteger > 32 and: [ self asInteger ~= 127 ])
		ifTrue: [ | tmp3 |
			tmp3 := arg1.
			tmp3
				nextPut: $$;
				nextPut: self ]
		ifFalse: [ tmp1 := self class constantNameFor: self.
			arg1 nextPutAll: self class name.
			tmp1 notNil
				ifTrue: [ | tmp5 |
					tmp5 := arg1.
					tmp5
						space;
						nextPutAll: tmp1 ]
				ifFalse: [ | tmp7 |
					tmp7 := arg1.
					tmp7
						nextPutAll: ' value: ';
						print: self asInteger ] ]