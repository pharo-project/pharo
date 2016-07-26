protocol
title: aString

	^ self widget
		ifNotNil: [:w | w title: aString ]