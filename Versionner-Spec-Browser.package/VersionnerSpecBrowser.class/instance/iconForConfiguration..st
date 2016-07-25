accessing
iconForConfiguration: aConfiguration

	"should return an IconMorph"
	^ (aConfiguration isDirty)
					ifTrue:  [ self class configurationChangedIcon] 
					ifFalse: [ self class configurationLoadedIcon ].