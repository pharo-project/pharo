menus behavior
buildInitializeCodeFor: aClass 

	^ String streamContents: [:str | 
		str << 'initialize';cr.
		aClass isMeta ifFalse: [str cr tab << 'super initialize.';cr.].
		aClass instVarNames sorted do: [ :name | 
			str cr tab << name << ' := nil.' ]].
	