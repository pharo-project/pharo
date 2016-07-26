action
setStyleValuesFrom: aFont
	((aFont isKindOf: LogicalFont) or:[aFont isKindOf: FontFamilyMemberAbstract])
		ifTrue:[
			weightValue := aFont weightValue.
			slantValue := aFont slantValue.
			stretchValue := aFont stretchValue]
		ifFalse:[
			weightValue := (aFont emphasis bitAnd: 1) > 0 ifTrue:[700] ifFalse:[400].
			slantValue := (aFont emphasis bitAnd: 2) > 0 ifTrue:[1] ifFalse:[0].
			stretchValue := 5 "normal"]