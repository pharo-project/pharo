examples - loops
exampleRepeatEffect
	<compilerOptions: #(#+ #optionInlineRepeat)>
	| i |
	i := 1.
	[ 
	i := i + 1.
	i = 10
		ifTrue: [ ^ true ] ] repeat