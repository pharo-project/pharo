generate scripts/methods
generateCS: packageInfo fromUpdate: updateNumber on: st
	
	st nextPutAll:
'"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

|repository|
repository := MCHttpRepository
                location: ''', self homeRepositoryUrlString,
'''
                user: ''''
                password: ''''.
(repository loadVersionFromFileNamed:' .
	st nextPut: $' ; nextPutAll: packageInfo, '.mcz'') load.'; cr.
	st nextPutAll: 'ScriptLoader new update', (updateNumber) asString; nextPutAll: '.' ; cr.
	st nextPutAll: '!'.
	^ st contents
