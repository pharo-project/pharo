user interface
fontButtonLabel
	| emphases |
	emphases := IdentityDictionary new at: nil put: 'Regular' translated;
				 at: 0 put: 'Regular' translated;
				 at: 1 put: 'Bold' translated;
				 at: 2 put: 'Italic' translated;
				 at: 4 put: 'Underlined' translated;
				 at: 8 put: 'Narrow' translated; 
				 at: 16 put: 'StruckOut' translated;
				 yourself.
	^ StringMorph contents: self realValue familyName , ' '
			, (emphases
					at: self realValue emphasis
					ifAbsent: ['']) , ' ' , self realValue pointSize asString font: self realValue