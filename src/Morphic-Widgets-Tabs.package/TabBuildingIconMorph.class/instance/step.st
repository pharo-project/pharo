stepping and presenter
step
	progress := progress + 1.
	progress > 10 ifTrue: [ progress := 1 ].
	
	progressIconMorph form: self currentIcon