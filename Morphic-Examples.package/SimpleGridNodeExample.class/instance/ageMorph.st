as yet unclassified
ageMorph
	^ ( self theme  
				newTextEntryIn: World
				for: self 
				get: #age
				set: #age:
				class: Integer
				getEnabled: nil
				help: nil) color: Color transparent