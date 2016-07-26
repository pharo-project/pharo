private
guessClassVarClass: aSHRange 
	| aClass name |
	name := self sourceOf: aSHRange.
	aClass := controller guessTypeFor: name.
	aClass
		ifNotNil: [^ aClass].
	^ (NECClassVarTypeGuesser 
		variableName: name 
		class: theClass theMetaClass) perform