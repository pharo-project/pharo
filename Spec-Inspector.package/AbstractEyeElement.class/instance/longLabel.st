accessing
longLabel
	"This is used by EyeTreeInspector"
	
	| description |
	description := self description.
	description size > 150
		ifTrue: [ description := description first: 150 ].
	^ self label 
		ifNotNil: [ :label | label , ': ' , description ] 
		ifNil: [ description ]