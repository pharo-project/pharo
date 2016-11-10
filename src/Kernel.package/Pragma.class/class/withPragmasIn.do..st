private
withPragmasIn: aClass do: aBlock
	aClass methodsDo: [ :method | method pragmas do: aBlock ]