accessing
paragraphProvider
	| o |
	o := owner.
	[ o notNil ]
		whileTrue: [ 
			o isParagraphProvider
				ifTrue: [ ^ o ].
			o := o owner ].
	^ nil