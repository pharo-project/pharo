like fullCopy
checkDeep
	"Write exceptions in the Transcript.  Every class that implements veryDeepInner: must copy all its inst vars.  Danger is that a user will add a new instance variable and forget to copy it.  This check is only run by hand once in a while to make sure nothing was forgotten.  
(Please do not remove this method.)
	DeepCopier new checkDeep 	"

	Transcript
		cr;
		show: 'Instance variables shared with the original object when it is copied'.
	(self systemNavigation allClassesImplementing: #veryDeepInner:) do: 
		[:aClass | | mm |
		(mm := aClass instVarNames size) > 0 ifTrue:
			[aClass instSize - mm + 1 to: aClass instSize do: 
				[:index | 
				((aClass compiledMethodAt: #veryDeepInner:) writesField: index) ifFalse: 
					[Transcript
						cr;
						show: aClass name;
						space;
						show: (aClass allInstVarNames at: index)]]]]