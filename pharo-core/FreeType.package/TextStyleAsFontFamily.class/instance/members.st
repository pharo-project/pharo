accessing
members
	^members ifNil:[
		members := #('Regular' 'Italic' 'Bold' 'Bold Italic')
		  collect:[:each | 
			TextStyleAsFontFamilyMember new 
				family: self;
				styleName: each;
				yourself]]