menus behavior
buildSetUpCodeFor: aClass 
	
	^ String streamContents: [:str | str << 'setUp';cr.
		str tab << '"Setting up code for '<<aClass name<<'"';cr;cr.
		aClass instVarNames sorted do: [:name | str cr tab << name << ' := nil.' ]].
	