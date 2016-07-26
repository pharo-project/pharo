private
newHeaderMorph
	^ (self theme 
			newRowIn: self theme
			for: {self tabSelectorMorph . self toolbarMorph}) cellInset: 0; cellPositioning: #bottomCenter; borderWidth: 0