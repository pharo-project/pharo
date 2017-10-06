accessing - morphs
firstNameMorph
	^ ( self theme  
				newTextEntryIn: World
				for: self 
				get: #firstName
				set: #firstName:
				class: String
				getEnabled: nil
				help: nil) color: Color transparent