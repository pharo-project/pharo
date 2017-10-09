*metacello-filetree
metacelloProjectClassFor: aScriptEngine
    aScriptEngine versionString isEmptyOrNil
        ifFalse: [ 
            "If we have a version in the script, then a BaselineOf is not being referenced...use a MetacelloMCProject
	    	 see https://github.com/dalehenrich/metacello-work/issues/7"
            ^ super metacelloProjectClassFor: aScriptEngine ].
    ^ MetacelloMCBaselineProject