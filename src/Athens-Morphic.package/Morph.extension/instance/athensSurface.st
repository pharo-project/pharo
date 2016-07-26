*Athens-Morphic
athensSurface

	^ owner 
		ifNil: [  nil ]
		ifNotNil: [ owner athensSurface  ]