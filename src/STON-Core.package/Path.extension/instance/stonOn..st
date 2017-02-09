*ston-core
stonOn: stonWriter
	stonWriter 
		writeObject: self 
		streamShortList: [ :listWriter | 
			self do: [ :each | listWriter add: each ] ]