filtering
keepSettingDeclaration: aSettingDeclaration
	^ ({aSettingDeclaration name. aSettingDeclaration label. aSettingDeclaration description}
				collect: [:e | e ifNil: ['']])
				anySatisfy: [:token | token  matchesRegexIgnoringCase: self regexp]
