private
classNamed: className

	(package includesClassNamed: className) 
		ifTrue:[ ^ package classNamed: className ].
	^ self createClassNamed: className