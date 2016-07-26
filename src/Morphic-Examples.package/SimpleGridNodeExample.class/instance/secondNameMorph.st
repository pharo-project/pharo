as yet unclassified
secondNameMorph
	^ ( self theme  
				newTextEntryIn: World
				for: self 
				get: #secondName
				set: #secondName:
				class: String
				getEnabled: nil
				help: nil) color: Color transparent