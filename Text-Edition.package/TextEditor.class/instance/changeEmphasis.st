menu messages
changeEmphasis

	|  reply code align menuList startIndex attribute choices |
	choices := self emphasisChoices.
	startIndex := self startIndex.
	align := self text alignmentAt: startIndex ifAbsent: [ 0 ].
	code := self text emphasisAt: startIndex.
	menuList := WriteStream on: Array new.
	menuList nextPut: (code isZero ->  'normal' translated).
	menuList nextPutAll: (choices  collect: [:emph|
							(code anyMask: (TextEmphasis perform: emph) emphasisCode)
								-> emph asString translated ]).
						
	menuList nextPut: ((self text attributesAt: startIndex)
							anySatisfy: [ :attr | attr isKern and: [attr kern < 0 ]]) -> 'narrow' translated.
	reply := UIManager default chooseFrom:  choices values: choices.
	reply notNil 
		ifTrue: [ attribute := TextEmphasis perform: reply.
				self applyAttribute:  attribute].
	^ true