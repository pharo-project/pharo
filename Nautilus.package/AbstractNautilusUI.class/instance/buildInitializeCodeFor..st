menus behavior
buildInitializeCodeFor: aClass 

	^ String streamContents: [:str | 
		str << 'initialize';cr.
		aClass isMeta ifFalse: [str cr tab << 'super initialize.';cr.].
		aClass instVarNames copy sort do: [ :name | 
			str cr tab << name << ' := nil.' ]].
	