transforming
newNameForSelf
	| variableName index originalName nonMetaClass |
	nonMetaClass := inlineClass theNonMetaClass.
	variableName := originalName := (nonMetaClass name first isVowel 
		ifTrue: [ 'an' ]
		ifFalse: [ 'a' ]) , nonMetaClass name.
	index := 1.
	
	[ variableName := self safeVariableNameBasedOn: variableName.
	inlineParseTree allDefinedVariables includes: variableName ] whileTrue: 
		[ variableName := originalName , index printString.
		index := index + 1 ].
	^ variableName