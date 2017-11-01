Extension { #name : #Set }

{ #category : #'*OpalCompiler-Core' }
Set >> parseOptions: anArray [

	"parse an array, which is a sequence of options in a form of: 
	
	#( + option1 option2 - option3 ... )
	
	each time the #+ is seen, the options which follow it will be subject for inclusion
	and, correspondingly, if #- seen, then they will be excluded	.

	By default, (if none of #+ or #- specified initially), all options are subject for inclusion.
	"
	| include |
	
	include := true.
	anArray do: [:option |
		option == #+ 
			ifTrue: [ include := true ]
			ifFalse: [
				option == #- 
					ifTrue: [ include := false ]
					ifFalse: [ 
						include ifTrue: [ self add: option ] 
						ifFalse: [ self remove: option ifAbsent:[] ]]]
		].
]
