filtering
keepSettingDeclaration: aSettingDeclaration
	| selector |
	selector := (aSettingDeclaration respondsTo: #getSelector) 
		ifTrue: [aSettingDeclaration getSelector] ifFalse: [''].
	^ (({aSettingDeclaration name.  
		aSettingDeclaration label. 
		selector asUppercase. 
		aSettingDeclaration description}
				collect: [:e | (e ifNil: ['']) asUppercase])
				includesSubstringAnywhere: self token asUppercase)
