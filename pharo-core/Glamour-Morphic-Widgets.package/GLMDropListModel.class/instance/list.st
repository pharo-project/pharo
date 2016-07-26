accessing
list

	^list ifNil: [ list:= (self glamourPresentation displayValue 
									collect:[ :each| (self glamourPresentation formatedDisplayValueOf: each) asMorph ])]