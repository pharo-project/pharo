text area behavior
getText
	| item | 
	
	item := self getPluginClassesList at: self pluginClassesSelectedIndex ifAbsent: [ ^ nil ].
	^ item 
		ifNil: [ '' ]
		ifNotNil: [ item description ]