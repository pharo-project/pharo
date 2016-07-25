accessing
label
	^ (self guessTypeWith: nil) ifNil:[ 'method' ] ifNotNil: [ 'class' ].