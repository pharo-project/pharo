menu messages
changeEmphasisOrAlignment
	| aList reply  code align menuList startIndex attribute |
	startIndex := self startIndex.
	align := self text alignmentAt: startIndex ifAbsent: [ 0 ].
	code := self text emphasisAt: startIndex.
	menuList := WriteStream on: Array new.
	menuList nextPut: (code isZero -> 'normal' translated).
	menuList nextPutAll: (#(bold italic underlined struckOut) collect: [:emph|
		(code anyMask: (TextEmphasis perform: emph) emphasisCode) -> emph asString translated]).
	
	menuList nextPut: ((self text attributesAt: startIndex)
		anySatisfy: [ :attr | attr isKern and: [attr kern < 0 ]]) -> 'narrow' translated.
		
	self alignmentChoices collectWithIndex: [ :type :i |
		menuList nextPut: (align = (i-1)) -> type asString translated ].
	
	aList := #(normal bold italic underlined struckOut narrow leftFlush centered rightFlush justified).
	reply := UIManager default chooseFrom:  aList values: aList.
	reply notNil ifTrue: [
		(self alignmentChoices includes: reply)
			ifTrue: [ attribute :=TextAlignment perform: reply]
			ifFalse: [ attribute := TextEmphasis perform: reply].
		self applyAttribute:  attribute].
	^ true