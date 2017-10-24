defaults
buttonLabelInsetFor: aButton
	"Answer the inset to use for a button's label."

	|left right|
	^aButton wantsRoundedCorners
		ifTrue: [left := ((aButton roundedCorners includesAnyOf: #(1 2)) ifTrue: [8] ifFalse: [4]).
				right := ((aButton roundedCorners includesAnyOf: #(3 4)) ifTrue: [8] ifFalse: [4]).
				Margin left: left top: 2 right: right bottom: 2]		
		ifFalse: [3@2]