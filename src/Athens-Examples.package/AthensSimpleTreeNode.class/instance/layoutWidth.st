accessing
layoutWidth 

	^ layoutWidth ifNil: [  
		| childWidth |

		childWidth := 0.
		children isEmpty ifFalse: [  
			children do: [ :each | childWidth := childWidth + each layoutWidth  ].
			childWidth := childWidth + (self spacingBetweenChilds * (children size-1)).
		].

		layoutWidth := self width max: childWidth.
	].
