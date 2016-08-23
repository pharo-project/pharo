menus behavior
buildTearDownCodeFor: aClass 
	
	^ String streamContents: [:str | str << 'tearDown';cr.
		str tab << '"Tearing down code for '<<aClass name<<'"';cr;cr.
		aClass instVarNames sorted do: [:name | str cr tab << name << ' := nil.' ]].
	